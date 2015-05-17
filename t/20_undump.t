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

use Data::Dumper 'Dumper';

our $HAVE_DATA_DUMP;
BEGIN { $HAVE_DATA_DUMP = eval q{ use Data::Dump 'pp'; 1 } };  ## no critic (ProhibitStringyEval)
diag  $HAVE_DATA_DUMP ? "Note: have Data::Dump" : "DON'T have suggested module Data::Dump";

BEGIN {
	use_ok 'Data::Undump::PPI';
}

my @TESTS = (
	[ "Hello" ],
	[ "Hello", "World" ],
	[ map {$_*3} 1..20 ], #TODO Later: Support range operator (Data::Dump generates code with it)
	[ {foo=>"bar",quz=>"baz"} ],
	[ [qw/abc def/] ],
	[ {foo=>"bar"}, ["quz","baz"], "blah" ],
	[ "A\x00B", "C\x11D", "E\x7FF" ],
	[ { foo=>[-abc,{def=>123},["hello",1,-7e8,9.8001]],
	  bar=>{deep=>{hash=>{refs=>567},blah=>[444]}} } ],
	#TODO Later: self-referential data structures
);

sub testundump ($$;$) {  ## no critic (ProhibitSubroutinePrototypes)
	my ($string,$data,$name) = @_;
	$name ||= "Undump";
	my @parsed = eval { Undump($string) };
	my $e = $@ ? "\$\@=$@" : "(no \$\@)\n";
	is_deeply \@parsed, $data, $name
		or diag explain "data=",$data, "str=$string\n", $e, "parsed=",\@parsed;
	return;
}

for my $test (@TESTS) {
	testundump(Dumper(@$test),$test,"Undump Data::Dumper");
	{
		local $Data::Dumper::Useqq=1;
		testundump(Dumper(@$test),$test,"Undump Data::Dumper w/ Useqq");
	}
	{
		local $Data::Dumper::Purity=1;
		testundump(Dumper(@$test),$test,"Undump Data::Dumper w/ Purity");
	}
	{
		local $Data::Dumper::Deepcopy=1;
		testundump(Dumper(@$test),$test,"Undump Data::Dumper w/ Deepcopy");
	}
	if (@$test==1) { # Terse only produces valid Perl with one value
		local $Data::Dumper::Terse=1;
		testundump(Dumper(@$test),$test,"Undump Data::Dumper w/ Terse");
	}
	if ($HAVE_DATA_DUMP) {
		testundump(pp(@$test),$test,"Undump Data::Dump");
	}
}

# test a few passthru cases
testundump(q{ $foo="bar"; },[{'$foo'=>'bar'}],"Undump passthru 1");
testundump(q{ $foo="bar"; $quz=[1,3,7]; },[{'$foo'=>'bar','$quz'=>[1,3,7]}],"Undump passthru 2");
testundump(q{ $foo="bar"; {quz=>'baz'}; },[{'$foo'=>'bar','_'=>[{quz=>"baz"}]}],"Undump passthru 3");

# test warns
my @w1 = warns {
		no warnings 'FATAL'; use warnings;  ## no critic (ProhibitNoWarnings)
		is_deeply [Undump('"foo"',"bar")], ["foo"], 'undump warn test';
	};
is @w1, 1, 'Undump extra args warn count';
like $w1[0], qr/\bignoring extra arguments to Undump\b/i, 'Undump extra args warn';

done_testing;

