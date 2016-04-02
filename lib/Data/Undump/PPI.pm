#!perl
package Data::Undump::PPI;
use warnings;
use strict;

our $VERSION = '0.04';

=head1 Name

Data::Undump::PPI - Perl extension for limited undumping of data structures
(via PPI, not eval)

=head1 Synopsis

=for comment
Remember to test this by copy/pasting to/from 91_author_pod.t

 use Data::Dumper;
 use Data::Undump::PPI;             # exports the "Undump()" function
 $Data::Dumper::Purity=1;           # should always be turned on for Undump
 
 my @input = ( {foo=>"bar"}, ["Hello","World"], "undumping!" );
 my $str = Dumper(@input);          # dump the data structure to a string
 my @parsed = Undump($str);         # parse the data structure back out
 
 # @parsed now looks identical to @input (is a deep copy)

=head1 Description

This module allows for I<limited> undumping and round-tripping of data
structures from strings generated by L<Data::Dumper|Data::Dumper>,
with some support for L<Data::Dump|Data::Dump> and possibly others.
It is a thin wrapper around L<Config::Perl|Config::Perl>, so please
see L<Config::Perl> for more details, including the limitations.

B<< This module exports a single function, C<Undump>, >> which accepts a
string and attempts to return the data as it would have been passed to
L<Data::Dumper|Data::Dumper>'s C<Dumper>, or L<Data::Dump|Data::Dump>'s C<dump> functions.
This means that for example, the C<$VAR1> variable names generated by
C<Dumper> will be removed and the list passed to C<Dumper(...)> is returned.
If the string doesn't look like the output of one of the dumper modules,
the output of L<Config::Perl|Config::Perl>'s C<parse_or_die> will be passed through.
C<Undump> will C<die> if it encounters problems.

B<< When using L<Data::Dumper|Data::Dumper>, >> make sure to always turn on its
C<Purity> option and turn off its C<Terse> option,
as otherwise L<Data::Dumper|Data::Dumper> may produce code that may not evaluate
back to the same data structure, sometimes even though it's valid, parseable Perl!

This module aims to support most of L<Data::Dumper|Data::Dumper>'s features
- except, notably, code references.
If you find a L<Data::Dumper|Data::Dumper> data structure that this module
does not yet support, please feel free to send in your data structure, as
it can help extend L<Config::Perl|Config::Perl>'s features and help fix bugs.
Currently, using modules other than L<Data::Dumper|Data::Dumper> may not work,
for example, L<Data::Dump|Data::Dump> sometimes generates code with the C<..>
range operator, which is currently not supported by L<Config::Perl|Config::Perl>.
In the future, this module's features may be extended to more fully support
dumper modules like L<Data::Dump|Data::Dump> as well.

Although L<Config::Perl|Config::Perl> now supports self-referential data
structures, you can also use L<Data::Dumper|Data::Dumper>'s C<Deepcopy>
option to get rid of references within data structures,
if the loss of references and copying of data is acceptable for your application.

This module is part of the L<Config::Perl|Config::Perl> distribution,
but was named separately in an attempt to make its purpose more clear
and its name a little easier to remember.

This document describes version 0.04 of the module.
Although this module has a fair number of tests, it still lacks some
features (see L<Config::Perl|Config::Perl>) and there may be bugs lurking.
Contributions are welcome!

=head1 Author, Copyright, and License

Copyright (c) 2015 Hauke Daempfling (haukex@zero-g.net).

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl 5 itself.

For more information see the L<Perl Artistic License|perlartistic>,
which should have been distributed with your copy of Perl.
Try the command "C<perldoc perlartistic>" or see
L<http://perldoc.perl.org/perlartistic.html>.

=cut

use Carp;
use Exporter 'import';

our @EXPORT = qw(Undump);  ## no critic (ProhibitAutomaticExportation)

use Config::Perl;

sub Undump {
	my ($in) = shift;
	warnings::warnif('Config::Perl',"ignoring extra arguments to Undump") if @_;
	
	my $parsed = Config::Perl->new->parse_or_die(\$in);
	my @keys = keys %$parsed;
	
	# does this look like Data::Dumper output?
	my $data_dumper=1;
	/^\$VAR\d+$/ or $data_dumper=0 for @keys;
	# if yes, sort the $VAR\d+ variables correctly
	$data_dumper and return
		map { $$parsed{ $$_[0] } }
		sort { $$a[1] <=> $$b[1] }
		map { [$_, /^\$VAR(\d+)$/] }
		@keys;
	
	# is the output a single value?
	# then it's likely Data::Dump, or Data::Dumper with Terse option
	if (@keys==1 && $keys[0] eq '_') {
		return @{ $$parsed{_} };
	}
	
	# none of the above, just pass through output
	return $parsed;
}


1;

