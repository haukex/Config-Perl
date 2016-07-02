#!perl
package Data::Undump::PPI;
use warnings;
use strict;

our $VERSION = '0.07';

=head1 Name

Data::Undump::PPI - Perl extension for limited undumping of data structures
(via PPI, not eval)

=head1 Synopsis

=for comment
Remember to test this by copy/pasting to/from 91_author_pod.t

 use Data::Dumper;
 use Data::Undump::PPI;             # "Undump()" is exported by default
 $Data::Dumper::Purity=1;           # should always be turned on for Undump
 
 my @input = ( {foo=>"bar"}, ["Hello","World"], "undumping!" );
 my $str = Dumper(@input);          # dump the data structure to a string
 my @parsed = Undump($str);         # parse the data structure back out
 # @parsed now looks identical to @input (is a deep copy)
 
 use Data::Undump::PPI qw/Dump Undump/;      # additionally import "Dump()"
 Dump(\@input, file=>'/tmp/test.conf');      # Data::Dumper to file
 my @conf = Undump(file=>'/tmp/test.conf');  # Undump directly from file

=head1 Description

This module allows for I<limited> undumping and round-tripping of data
structures from strings generated by L<Data::Dumper|Data::Dumper>
(and possibly other dumper modules, but that's currently not explicitly supported).
It is a thin wrapper around L<Config::Perl|Config::Perl>, so please
see L<Config::Perl> for more details, including the limitations.

When using L<Data::Dumper|Data::Dumper>, make sure to always turn on its
C<Purity> option and turn off its C<Terse> option, as otherwise
L<Data::Dumper|Data::Dumper> may produce code that may not evaluate
back to the same data structure, sometimes even though it's valid,
parseable Perl! See also L</Dump> for a helper function.

=head2 C<Undump>

 my @out = Undump($string);
 my @out = Undump(file => $filename);
 my @out = Undump(fh => $filehandle);

Accepts either a string, a filename or a filehandle, parses it and
attempts to return the data as it would have been passed to
L<Data::Dumper|Data::Dumper>'s C<Dumper>. This means that the C<$VAR1>,
C<$VAR2> etc. variable names generated by C<Dumper> will be removed and the
argument list passed to C<Dumper(...)> is returned, and if the string ends
on a true value, that will be ignored so that parsing files which end on
e.g. C<1;>, like those generated by the L</Dump> helper function, will work.

In list context, this function returns the list of values as they would
have been passed as the arguments to C<Dumper(...)>. If you know that the
data contains only one value, you may call C<Undump> in scalar context to
get that value.
If you call C<Undump> in scalar context but the data contains more than
one value, currently the I<last> value is returned and a warning is issued
(in the "Config::Perl" warnings category).

If the string doesn't look like the output of L<Data::Dumper|Data::Dumper>,
this function will throw an exception, and any errors from
L<Config::Perl|Config::Perl>'s C<parse_or_die> will also be passed through.

If you used L<Data::Dumper|Data::Dumper>'s ability to give the dumped
variables user-specified names, you will need to use
L<Config::Perl|Config::Perl> to parse that, since C<Undump> only supports
the C<$VAR...> style output of C<Dumper>.

This function is exported by default.

=head2 C<Dump>

 my $str = Dump(\@data);
 Dump(\@data, file => $filename);
 Dump(\@data, fh => $filehandle);
 Dump(\@data, ..., %dumper_options);  # e.g.:
 Dump(\@data, file => $filename, Deepcopy=>1);

This function is a simple helper for L<Data::Dumper|Data::Dumper> which
sets some default options and always returns a string, optionally writing
that string to a file or filehandle.

The L<Data::Dumper|Data::Dumper> options that can be set are:
C<Deepcopy> (default: off),  C<Useqq> (default: on),
C<Quotekeys> (default: off), C<Sortkeys> (default: on), and
C<Indent> (default is L<Data::Dumper|Data::Dumper>'s default).
Note that C<Terse> is always off and C<Purity> is always on.

When writing to a file, the output is prefixed with C<#!perl> and
ended with C<1;>, this is I<not> the case when writing to a filehandle.

=head2 More Details

This module aims to support most of L<Data::Dumper|Data::Dumper>'s features
except code references and (currently) C<bless>ed objects.
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

This document describes version 0.07 of the module.
B<This is a development version.>
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

our @EXPORT = qw/Undump/;  ## no critic (ProhibitAutomaticExportation)
our @EXPORT_OK = qw/Undump Dump/;
our %EXPORT_TAGS = ( ':all' => [qw/Undump Dump/] );

use Config::Perl;
use Data::Dumper ();

sub Undump {  ## no critic (RequireArgUnpacking)
	my ($in);
	if (@_==1)
		{ $in = \( shift ) }
	elsif (@_==2) {
		my ($k,$v) = @_;
		if ($k eq 'file')
			{ $in = $v }
		elsif ($k eq 'fh') {
			local $/ = undef;
			$in = \( scalar <$v> );
		}
		else
			{ croak "Undump: unknown option \"$k\"" }
	}
	else
		{ croak "bad number of arguments to Undump" }
	croak "Undump was passed an undef value"
		unless defined $in;
	
	my $parsed = Config::Perl->new->parse_or_die($in);
	my $data_dumper=1; # does this look like Data::Dumper output?
	/^(?:\$VAR\d+|_)$/ or $data_dumper=0 for keys %$parsed;
	if (exists $$parsed{_}) {
		$data_dumper=0 unless @{ $$parsed{_} }==1 && $$parsed{_}[0];
		delete $$parsed{_};
	}
	croak "input doesn't look like Data::Dumper output"
		unless $data_dumper;
	# sort the $VAR\d+ variables correctly
	my @out =
		map { $$parsed{ $$_[0] } }
		sort { $$a[1] <=> $$b[1] }
		map { [$_, /^\$VAR(\d+)$/] }
		keys %$parsed;
	if (wantarray)
		{ return @out }
	else {
		warnings::warnif('Config::Perl','Undump was called in scalar context '
			.'but data contained more than one value') if @out>1;
		return $out[-1]; # behave like comma operator in scalar context
	}
}

sub Dump {  ## no critic (RequireArgUnpacking)
	my $data = shift;
	croak "first argument to Dump must be an arrayref"
		unless ref $data eq 'ARRAY';
	my %opts = @_;
	my %KNOWN_OPTS = map {$_=>1} qw/ fh file Deepcopy Useqq Quotekeys Sortkeys Indent /;
	exists $KNOWN_OPTS{$_} or croak "Dump: unknown option \"$_\""
		for keys %opts;
	croak "Dump: options fh and file may not be used together"
		if defined $opts{fh} && defined $opts{file};
	
	my $dd = Data::Dumper->new($data);
	$dd->Deepcopy (defined $opts{Deepcopy}  ? $opts{Deepcopy}  : 0);
	$dd->Useqq    (defined $opts{Useqq}     ? $opts{Useqq}     : 1);
	$dd->Quotekeys(defined $opts{Quotekeys} ? $opts{Quotekeys} : 0);
	$dd->Sortkeys (defined $opts{Sortkeys}  ? $opts{Sortkeys}  : 1);
	$dd->Indent($opts{Indent}) if defined $opts{Indent};
	$dd->Purity(1)->Terse(0);
	my $str = $dd->Dump;
	# I think Data::Dumper always ends on a semicolon, but just to be paranoid...
	$str .= ";" unless $str=~/;\s*$/;
	
	my $fh = $opts{fh};
	if (defined $opts{file}) {
		open $fh, '>', $opts{file}  ## no critic (RequireBriefOpen)
			or croak "Dump couldn't open \"$opts{file}\" for writing: $!";
		print $fh "#!perl\n";
	}
	if (defined $fh) {
		print $fh $str;
		# I think Data::Dumper always ends on a newline, but just to be paranoid...
		print $fh "\n" unless $str=~/\n\z/;
	}
	if (defined $opts{file}) {
		print $fh "1;\n";
		close $fh;
	}
	
	return $str;
}


1;

