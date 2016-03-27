#!perl
package Data::Undump::PPI;
use warnings;
use strict;

our $VERSION = '0.03';

=head1 Name

Data::Undump::PPI - Perl extension for limited undumping of data structures via PPI

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
structures from strings generated by modules such as
L<Data::Dumper|Data::Dumper> and L<Data::Dump|Data::Dump>.
It is a thin wrapper around L<Config::Perl|Config::Perl>, so please
see L<Config::Perl> for more details, including the limitations.

This module exports a single function, C<Undump>, which attempts to
return the data as it would have been passed to
L<Data::Dumper|Data::Dumper>'s C<Dumper> or L<Data::Dump|Data::Dump>'s C<dump> functions.
This means that for example, the C<$VAR1> variables generated by C<Dumper> will be stripped.
If the string doesn't look like the output of one of the dumper modules,
the output of L<Config::Perl|Config::Perl>'s C<parse_or_die> will be passed through.
C<Undump> will C<die> if it encounters problems.

Because at the moment L<Config::Perl|Config::Perl> has only very limited support
for references, self-referential data structures will most likely not work
(support may be added in a later release of L<Config::Perl|Config::Perl>).
For now, a possible workaround may be L<Data::Dumper|Data::Dumper>'s C<Deepcopy> option,
if the loss of references and copying of data is acceptable for your application.
Note that you should always turn on L<Data::Dumper|Data::Dumper>'s C<Purity> option,
as otherwise L<Data::Dumper|Data::Dumper> may produce code that may not evaluate
back to the same data structure, even though it's valid, parseable Perl!

If you're using L<Data::Dump|Data::Dump>, note that some of the code it generates
is currently unsupported by L<Config::Perl|Config::Perl>, such as the range operator C<..>.
Because of this, you may be better off using L<Data::Dumper|Data::Dumper> for now.

If you're using L<Data::Dumper|Data::Dumper>, note that its C<Terse>
option may cause C<Dumper> to generate invalid Perl strings if you pass it
more than one value.

This module is part of the L<Config::Perl|Config::Perl> distribution,
but was named seperately in an attempt to make its purpose more clear
and its name a little easier to remember.

This document describes version 0.03 of the module.
B<This is a development version.>
Although this module is well-tested and working, it still lacks some
features to make it I<really> useful (see L<Config::Perl|Config::Perl>).
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

