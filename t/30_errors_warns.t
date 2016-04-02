#!/usr/bin/env perl
use warnings;
use strict;

# Tests for the Perl module Config::Perl
# 
# Copyright (c) 2015 Hauke Daempfling (haukex@zero-g.net).
# 
# This library is free software; you can redistribute it and/or modify
# it under the same terms as Perl 5 itself.
# 
# For more information see the "Perl Artistic License",
# which should have been distributed with your copy of Perl.
# Try the command "perldoc perlartistic" or see
# http://perldoc.perl.org/perlartistic.html .

use FindBin ();
use lib $FindBin::Bin;
use Config_Perl_Testlib;

use Test::More;
use Test::Fatal 'exception';

use Config::Perl;
use Data::Undump::PPI;

my $cp = Config::Perl->new;

# Error Tests

like exception { Config::Perl->new('foo','bar') },
	qr/\bunknown argument\b/, 'bad args to new';

like exception { $cp->parse_or_die(\' substr("foo" ') },
	qr/\bdocument incomplete\b/i, 'doc incomplete';

is $cp->parse_or_undef(\' substr("foo" '), undef, 'parse_or_undef with error';
like $cp->errstr, qr/\bdocument incomplete\b/i, 'parse_or_undef check errstr';

is $cp->parse_or_undef('this_file_really_shouldnt_exist'), undef, 'parse bad filename';
like $cp->errstr, qr/\bParse failed\b/i, 'bad filename errstr';

ok exception { $cp->parse_or_die(\q{m/abc/;}) }, 'parsing regexp fails';

ok exception { $cp->parse_or_die(\q{do;}) }, 'do w/o block';

like exception { $cp->parse_or_die(\q{"\f"}) },
	qr/\bDon't support escape sequence\b/, 'unsupported backslash escape';
like exception { $cp->parse_or_die(\q{"$."}) },
	qr/\bDon't support string interpolation of '\$\.'/i, 'unsupported interpolation';

like exception { $cp->parse_or_die(\q{local $x = "abc";}) },
	qr/\bUnsupported declaration type "local"/i, 'local decl unsupported';
like exception { $cp->parse_or_die(\q{ do { my $x = "abc"; } }) },
	qr/\bLexical variables .*not supported\b/, 'my decl in block unsupported';

like exception { Config::Perl->new->parse_or_die(\q{ @foo[3,4] }) },
	qr/\bExpected subscript to contain a single value\b/i, 'subscript multi value';

# Warning Tests

my @w1 = warns {
		test_ppconf q{ "foo"; $bar="quz"; }, { '$bar'=>"quz" }, 'value in void ctx';
	};
ok @w1>=1, 'value in void ctx warn count';
is grep({/\bvalue in void context\b/i} @w1), 1, 'value in void ctx warning';

my @w2 = warns {
		test_ppconf q{ do { "abc" }; 1 }, { '_' =>[ 1 ] }, 'do block void ctx';
	};
ok @w2>=1, 'do block void ctx warn count';
is grep({/\bvalue in void context\b/i} @w2), 1, 'do block void ctx warning';

my @w3 = warns {
	test_ppconf q{ our $baz="baz$xyz"; },
		{ '$baz'=>"baz" }, 'undef interp 1', {add_syms=>{'$xyz'=>\undef}};
	test_ppconf q{ our $xyz=undef; our $baz="baz$xyz"; },
		{ '$xyz'=>undef, '$baz'=>"baz" }, 'undef interp 2';
	};
ok @w3>=2, 'undef interp warn count';
is grep({/\bUse of uninitialized value \$xyz in interpolation\b/} @w3), 2, 'undef interp warn';

my @w4 = warns {
		no warnings 'FATAL'; use warnings;  ## no critic (ProhibitNoWarnings)
		is_deeply [Undump('"foo"',"bar")], ["foo"], 'undump warn test';
	};
ok @w4>=1, 'Undump extra args warn count';
is grep({/\bignoring extra arguments to Undump\b/i} @w4), 1, 'Undump extra args warn';




done_testing;

