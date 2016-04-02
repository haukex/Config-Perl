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
	# Purity should always be turned on
	local $Data::Dumper::Purity=1;
	testundump(Dumper(@$test),$test,"Undump Data::Dumper");
	{
		local $Data::Dumper::Useqq=1;
		testundump(Dumper(@$test),$test,"Undump Data::Dumper w/ Useqq");
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

# test a few passthru cases (should not be recognized as Data::Dumper or Data::Dump output)
testundump(q{ $foo="bar"; },[{'$foo'=>'bar'}],"Undump passthru 1");
testundump(q{ $foo="bar"; $quz=[1,3,7]; },[{'$foo'=>'bar','$quz'=>[1,3,7]}],"Undump passthru 2");
testundump(q{ $foo="bar"; {quz=>'baz'}; },[{'$foo'=>'bar','_'=>[{quz=>"baz"}]}],"Undump passthru 3");

# ### first test of Data::Dumper self-referential data structures ###
my $STRUCT = {
	foo => [ {x=>1,y=>2}, "blah" ],
	bar => { quz=>[7,8,9], baz=>"bleep!" },
	quz => [ { a=>[qw/b c d/,{x=>4},7], b=>{t=>["u","v","x"]} },
		[3,4,5,[6,7,8]], "h" ], };
$STRUCT->{refs} = [
	$STRUCT->{foo},
	$STRUCT->{foo}->[0],
	$STRUCT->{bar}->{quz},
	$STRUCT->{quz}->[0]->{a}->[3],
	$STRUCT->{quz}->[0]->{b}->{t},
	$STRUCT->{quz}->[1]->[3],
	];
my $str = do {
	local $Data::Dumper::Deepcopy = 0;
	local $Data::Dumper::Purity = 1;
	Dumper($STRUCT) };
$str .= <<'ENDMORE'; # a few more refs to individual values with specific formats
$x = $VAR1->{foo}->[1];
$y = $VAR1->{quz}->[0]->{b}->{t}->[1];
$z = $VAR1->{quz}->[1][3][2];
ENDMORE
note $str; #TODO debug, remove me
test_ppconf $str, {'$VAR1'=>$STRUCT, '$x'=>"blah", '$y'=>"v", '$z'=>8},
	'Data::Dumper complex self-ref. structure';
# ###

done_testing;

