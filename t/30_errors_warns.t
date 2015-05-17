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

my $cp = Config::Perl->new;

# Error Tests

like exception { Config::Perl->new('foo') },
	qr/\btakes no arguments\b/, 'new takes no args';

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
like exception { $cp->parse_or_die(\q{my $x = "abc";}) },
	qr/\bLexical variables .*not supported\b/, 'my decl unsupported';

like exception { Config::Perl->new->parse_or_die(\q{ @foo[3,4] }) },
	qr/\bExpected subscript to contain a single value\b/i, 'subscript multi value';


done_testing;

