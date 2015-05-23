#!perl
package Config::Perl;
use warnings;
use strict;

our $VERSION = '0.03';

=head1 Name

Config::Perl - Perl extension to parse configuration files written in a subset of Perl
and (limited) undumping of data structures (safer than eval thanks to parsing via PPI)

=head1 Synopsis

=for comment
Remember to test this by copy/pasting to/from 91_author_pod.t

 use Config::Perl;
 my $parser = Config::Perl->new;
 my $data = $parser->parse_or_die( \<<' END_CONFIG_FILE' );
   # This is the example configuration file
   $foo = "bar";
   %text = ( test => ["Hello", "World!"] );
   @vals = qw/ x y a /;
 END_CONFIG_FILE
 print $data->{'$foo'}, "\n";   # prints "bar\n"
 
 # Resulting $data: {
 #   '$foo'  => "bar",
 #   '%text' => { test => ["Hello", "World!"] },
 #   '@vals' => ["x", "y", "a"],
 # };

=head1 Description

The goal of this module is to support the parsing of a small subset of Perl,
primarily in order to parse configuration files written in that subset of Perl.
As a side effect, this module can "undump" some data structures written by
L<Data::Dumper|Data::Dumper> and L<Data::Dump|Data::Dump> - see L<Data::Undump::PPI>.

The code is parsed via L<PPI|PPI>, eliminating the need for Perl's C<eval>.
This should provide a higher level of safety* compared to C<eval>
(even when making use of a module like L<Safe|Safe>).

* B<Disclaimer:> A "higher level of safety" does not mean "perfect safety".
This software is distributed B<without any warranty>; without even the implied
warranty of B<merchantability> or B<fitness for a particular purpose>.
See also the licence for this software.

This module attempts to provide 100% compatibility with Perl over the subset of Perl it supports.
When a Perl feature is not supported by this module, it should complain 
that the feature is not supported, instead of silently giving a wrong result.
If the output of a parse is different from how Perl would evaluate the same string,
then that is a bug in this module that should be fixed by correcting the output
or adding an error message that the particular feature is unsupported.
However, the result of using this module to parse something that is not valid Perl is undefined;
it may cause an error, or may fail in some other silent way.

This document describes version 0.03 of the module.
B<This is a development version.>
Although this module is well-tested and working, it still lacks some
features to make it I<really> useful (see list below).
Contributions are welcome!

=head2 Interface

This module has a simple OO interface. A new parser is created
with C<< Config::Perl->new >>, which currently does not take any arguments,
and documents are parsed with either the method C<parse_or_die> or C<parse_or_undef>.

 my $parser = Config::Perl->new;
 my $out1 = $parser->parse_or_undef(\' $foo = "bar"; ');
 warn "parse failed: ".$parser->errstr unless defined $out1;
 my $out2 = $parser->parse_or_die('filename.pl');

The arguments and return values of these two methods are (almost) the same:
They each take exactly one argument, which is either a filename,
or a reference to a string containing the code to be parsed
(this is the same as L<PPI::Document|PPI::Document>'s C<new> method).

The methods differ in that, as the names imply, C<parse_or_die>
will C<die> on errors, while C<parse_or_undef> will return C<undef>;
the error message is then accessible via the C<errstr> method.

For a successful parse, the return value of each function is a hashref
representing the "symbol table" of the parsed document.
This "symbol table" hash is similar to, but not the same as, Perl's symbol table.
The hash includes a key for every variable declared or assigned to in the document,
the key is the name of the variable including its sigil.
If the document ends with a plain value or list that is not part of an assignment,
that value is saved in the "symbol table" hash with the key "C<_>" (a single underscore).

For example, the string C<"$foo=123; $bar=456;"> will return the data structure
C<< { '$foo'=>123, '$bar'=>456 } >>, and the string C<"('foo','bar')"> will return the data
structure C<< { _=>["foo","bar"] } >>.

Note that documents are currently always parsed in list context.
For example, this means that a document like "C<@foo = ("a","b","c"); @foo>"
will return the array's elements (C<"a","b","c">) instead of the item count (C<3>).
This also means that the special hash element "C<_>" will currently always be an arrayref.

=head2 What is currently supported

=over

=item *

plain scalars, arrays, hashes, lists

=item *

arrayrefs and hashrefs constructed via C<[]> and C<{}> resp.

=item *

declarations - only C<our>, also C<my> on the outermost level (document)
where it is treated exactly like C<our>;
not supported are lexical C<my> inside blocks, C<local> or C<state>

=item *

assignments (except the return value of assignments is not yet implemented)

=item *

simple array and hash subscripts (e.g. C<$x[1]>, C<$x[$y]>, C<$x{z}>, C<$x{"$y"}>)

=item *

very simple variable interpolations in strings (currently only C<"hello$world"> or C<"foo${bar}quz">)
and some escape sequences (e.g. C<"\x00">)

=item *

C<do> blocks (contents limited to the supported features listed here)

=back

=head2 What is not supported (yet)

I hope to achieve a balance where this module is useful, without becoming too much of a re-implementation of Perl.
I've labeled these items with "wishlist", "maybe", and "no", depending on whether I currently feel that
I'd like to support this feature in a later version, I'd consider supporting this feature if the need arises,
or I currently don't think the feature should be implemented.

=over

=item *

lexical variables (C<my>) (wishlist)

=item *

taking references via C<\> and dereferencing (C<@{...}>, C<%{...}>, etc.) (wishlist)

=item *

return values of assignments (e.g. C<$foo = do { $bar = "quz" }>) (maybe)

=item *

operators other than assignment (maybe; supporting a subset, like concatenation, is wishlist)

=item *

conditionals, like for example a very simple C<if ($^O eq 'linux') { ... }> (maybe)

=item *

any functions (mostly this is "no"; supporting a very small subset of functions, e.g. C<push>, is "maybe")

=item *

anything that can't be resolved via a static parse (including C<sub>s, many regexps, etc.) (no)

=item *

Note this list is not complete.

=back

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
use warnings::register;
use PPI ();
use PPI::Dumper ();

sub new {
	my $class = shift;
	croak "new currently takes no arguments" if @_;
	my $self = {
		errstr => undef,
		out => undef,
		ctx => undef, # Note: valid values for ctx currently "list", "scalar", "scalar-void"
	};
	return bless $self, $class;
}
sub errstr { return shift->{errstr} }

#TODO: make _errmsg a little prettier?
sub _dump { return PPI::Dumper->new(shift,whitespace=>0,comments=>0,locations=>0)->string }
sub _errmsg { chomp(my $e=_dump(shift)); $e=~s/^/\t/mg; return "<<< $e >>>" }

sub parse_or_undef {  ## no critic (RequireArgUnpacking)
	my $self = shift;
	my $out = eval { $self->parse_or_die(@_) };
	my $errmsg = $@||"Unknown error";
	$self->{errstr} = defined $out ? undef : $errmsg;
	return $out;
}

sub parse_or_die {
	my ($self,$input) = @_;
	# PPI::Documents are not "complete" if they don't have a final semicolon, so tack on on there if it's missing
	$input = \"$$input;" if ref $input eq 'SCALAR' && $$input!~/;\s*$/;
	$self->{doc} = my $doc = PPI::Document->new($input);
	my $errmsg = PPI::Document->errstr||"Unknown error";
	$doc or croak "Parse failed: $errmsg";
	$doc->complete or croak "Document incomplete (missing final semicolon?)";
	$self->{ctx} = 'list';
	$self->{out} = {};
	my @rv = $self->_handle_block($doc, outer=>1);
	$self->{out}{_} = \@rv if @rv;
	return $self->{out};
}

sub _handle_block {  ## no critic (ProhibitExcessComplexity)
	my ($self,$block,%param) = @_;
	confess "invalid \$block class"
		unless $block->isa('PPI::Structure::Block') || $block->isa('PPI::Document');
	return unless $block->schildren;
	my @rv;
	my $el = $block->schild(0);
	ELEMENT: while ($el) {
		# uncoverable branch true
		$el->isa('PPI::Statement') or croak "Unsupported element\n"._errmsg($el);
		my @sc = $el->schildren;
		# remove semicolons from statements
		if ( @sc && $sc[-1]->isa('PPI::Token::Structure') && $sc[-1]->content eq ';' )
			{ pop(@sc)->delete }
		next ELEMENT unless @sc; # empty statement?
		# last statement in block gets its context, otherwise void context
		local $self->{ctx} = $el->snext_sibling ? 'scalar-void' : $self->{ctx};
		my $is_assign; # remove this once _handle_assign return values implemented
		# variable declaration
		if ($el->class eq 'PPI::Statement::Variable') {
			# note that Perl does not allow array or hash elements in declarations
			# so we don't have to worry about subscripts here
			croak "Unsupported declaration type \"".$el->type."\""
				unless $el->type eq 'our' || $el->type eq 'my';
			croak "Lexical variables (\"my\") not supported" # I'd like to support "my" soon
				unless $el->type eq 'our' || ($el->type eq 'my' && $param{outer});
			# Note: Don't use $el->symbols, as that omits undefs on LHS!
			$self->_handle_assign($el,$sc[1],$sc[3]);
			$is_assign=1;
		}
		elsif ($el->class eq 'PPI::Statement') {
			# assignment, possibly with symbol+subscript on the RHS
			if ( (@sc==3||@sc==4) && $sc[1]->isa('PPI::Token::Operator') && $sc[1]->content eq '=' ) {  ## no critic (ProhibitCascadingIfElse)
				$self->_handle_assign($el,$sc[0],$sc[2]);
				$is_assign=1;
			}
			# assignment assumed to have a symbol+subscript on the LHS
			elsif ( (@sc==4||@sc==5) && $sc[2]->isa('PPI::Token::Operator') && $sc[2]->content eq '=' ) {
				$self->_handle_assign($el,$sc[0],$sc[3]);
				$is_assign=1;
			}
			# do-BLOCK
			elsif ( @sc==2 && $sc[0]->isa('PPI::Token::Word') && $sc[0]->literal eq 'do'
				&& $sc[1]->isa('PPI::Structure::Block') ) {
				my @tmprv = $self->_handle_block($sc[1]);
				@rv = @tmprv unless $self->{ctx} eq 'scalar-void';
			}
			# single statements
			elsif ( @sc==1 || (@sc==2 && $sc[0]->isa('PPI::Token::Symbol') && $sc[1]->isa('PPI::Structure::Subscript')) ) {
				my @tmprv = $self->_handle_value($sc[0]);
				@rv = @tmprv unless $self->{ctx} eq 'scalar-void';
				warnings::warnif("value in void context") if $self->{ctx} eq 'scalar-void';
			}
			# push
			elsif ( @sc>2 && $sc[0]->isa('PPI::Token::Word') && $sc[0]->literal eq 'push') {
				croak "don't support push\n"._errmsg($el); # I'm considering supporting push
			}
			else { croak "Unsupported element\n"._errmsg($el) }
		}
		elsif ( $el->isa('PPI::Statement::Compound') && @sc==1 && $sc[0]->isa('PPI::Token::Label') ) {
			# ignore labels
		}
		else { croak "Unsupported element ".$el->class." in\n"._errmsg($el) }
		if ($is_assign && $self->{ctx} ne 'scalar-void') {
			# special case: the last statement of the outermost block
			#TODO: Would it make sense to not error out on *any* assignment at the end of a block, not just the outermost one?
			if ($param{outer} && !$el->snext_sibling)
				{} # currently nothing; could warn here?
			else
				{ croak "Assignment return values not implemented (current context is $$self{ctx}) in\n"._errmsg($el) }
		}
	} continue { $el = $el->snext_sibling }
	return @rv;
}

# returns nothing (yet)
sub _handle_assign {
	my ($self,$as,$lhs,$rhs) = @_;
	confess "invalid \$as class"
		unless $as->class eq 'PPI::Statement' || $as->class eq 'PPI::Statement::Variable';
	# Note we expect our caller to pick the correct $lhs and $rhs children from $as,
	# and at the moment *some* call sites also already check the number of children.
	# Possible To-Do for Later: Clean up the _handle_assign calling
	croak "bad assignment statement length in:\n"._errmsg($as)
		if $as->schildren<3 || $as->schildren>5;
	
	my $lhs_scalar;
	my @lhs;
	if ($lhs->isa('PPI::Token::Symbol')) {
		@lhs = ($self->_handle_symbol($lhs));
		$lhs_scalar = $lhs[0]->{atype} eq '$';
	}
	elsif ($lhs->isa('PPI::Structure::List')) {
		local $self->{ctx} = 'list';
		@lhs = $self->_handle_list($lhs,is_lhs=>1);
	}
	else { confess "invalid assignment LHS "._errmsg($lhs) }  # uncoverable statement
	
	local $self->{ctx} = $lhs_scalar ? 'scalar' : 'list';
	my @rhs = $self->_handle_value($rhs);
	
	for my $l (@lhs) {
		if (!defined($l))  ## no critic (ProhibitCascadingIfElse)
			{ shift @rhs }
		elsif ($l->{atype} eq '$')
			{ ${ $l->{ref} } = shift @rhs }
		elsif ($l->{atype} eq '@') {
			if (!defined ${$l->{ref}})
				{ ${ $l->{ref} } = [@rhs] }
			else
				{ @{ ${ $l->{ref} } } = @rhs }
			last; # slurp
		}
		elsif ($l->{atype} eq '%') {
			if (!defined ${$l->{ref}})
				{ ${ $l->{ref} } = {@rhs} }
			else
				{ %{ ${ $l->{ref} } } = @rhs }
			last; # slurp
		}
		else { confess "Possible internal error: can't assign to "._errmsg($l)." in\n"._errmsg($as) }  # uncoverable statement
	}
	return;
}

# returns a list (if param is_lhs is true, list will consist of only _handle_symbol return values)
sub _handle_list {  ## no critic (ProhibitExcessComplexity)
	my ($self,$outerlist,%param) = @_;
	# NOTE this handles both () lists as well as the *contents* of {} and [] constructors
	confess "outerlist is undef?" unless $outerlist;
	confess "bad list class ".$outerlist->class
		unless $outerlist->isa('PPI::Structure::List') || $outerlist->isa('PPI::Structure::Constructor');
	# We should already have been placed in list context
	confess "Internal error: Context is not list? Is \"$$self{ctx} \"at:\n"._errmsg($outerlist)
		unless $self->{ctx}=~/^list\b/;
	croak "can only handle a plain list on LHS"
		if $param{is_lhs} && !$outerlist->isa('PPI::Structure::List');
	return unless $outerlist->schildren; # empty list
	# the first & only child of the outer list structure is a statement / expression
	my $act_list = $outerlist->schild(0);
	croak "Unsupported list\n"._errmsg($outerlist)
		unless $outerlist->schildren==1 && ($act_list->isa('PPI::Statement::Expression') || $act_list->class eq 'PPI::Statement');
	return unless $act_list->schildren; # empty list?
	my @thelist;
	my $expect = 'item';
	my $el = $act_list->schild(0);
	ELEMENT: while ($el) {
		if ($expect eq 'item') {
			my $peek_next = $el->snext_sibling;
			my $fat_comma_next = $peek_next && $peek_next->isa('PPI::Token::Operator') && $peek_next->content eq '=>';
			if ($param{is_lhs}) {
				if ($el->isa('PPI::Token::Symbol'))
					{ push @thelist, $self->_handle_symbol($el) }
				elsif (!$fat_comma_next && $el->isa('PPI::Token::Word') && $el->literal eq 'undef')
					{ push @thelist, undef }
				else
					{ croak "Don't support this on LHS: "._errmsg($el) }
			}
			else {
				# handle fat comma autoquoting words
				if ($fat_comma_next && $el->isa('PPI::Token::Word') && $el->literal=~/^\w+$/ )
					{ push @thelist, $el->literal }
				elsif ($el->isa('PPI::Token::QuoteLike::Words')) # qw// in a list
					{ push @thelist, $el->literal } # here "literal" returns a list of words
				else {
					push @thelist, $self->_handle_value($el);
					# special case of do followed by BLOCKs
					if ($el->isa('PPI::Token::Word') && $el->literal eq 'do'
						&& $peek_next && $peek_next->isa('PPI::Structure::Block'))
						{ $el = $el->snext_sibling } # this will have been handled by _handle_value
				}
			}
			# special case of symbols followed by subscripts
			# Possible To-Do for Later: More generalized handling of multi-element list items?
			#   Right now we have special handling of Symbol-Subscript and do-BLOCK pairs, if more special cases appear,
			#   we should figure out a more generalized way of advancing our list pointer over the handled elements.
			if ($el->isa('PPI::Token::Symbol') && $peek_next && $peek_next->isa('PPI::Structure::Subscript'))
				{ $el = $el->snext_sibling } # this will have been handled by _handle_symbol, called from _handle_value
			$expect = 'separator';
		}
		elsif ($expect eq 'separator') {
			croak "Expected list separator, got "._errmsg($el)
				unless $el->isa('PPI::Token::Operator')
				&& ($el->content eq ',' || $el->content eq '=>');
			$expect = 'item';
		}
		else { confess "really shouldn't happen, bad state $expect" }  # uncoverable statement
	} continue { $el = $el->snext_sibling }
	return @thelist;
}

# respects context and returns either a single value, or list if applicable
sub _handle_value {  ## no critic (ProhibitExcessComplexity)
	my ($self,$val) = @_;
	confess "\$val is false" unless $val;
	if ($val->isa('PPI::Token::Number'))  ## no critic (ProhibitCascadingIfElse)
		{ return 0+$val->literal }
	elsif ($val->isa('PPI::Token::Quote'))
		{ return $self->_handle_quote($val) }
	elsif ($val->isa('PPI::Structure::Constructor'))
		{ return $self->_handle_struct($val) }
	elsif ($val->isa('PPI::Token::Word') && $val->literal eq 'undef')
		{ return undef }  ## no critic (ProhibitExplicitReturnUndef)
	elsif ($val->isa('PPI::Token::Word') && $val->literal=~/^-\w+$/)
		{ return $val->literal }
	elsif ($val->isa('PPI::Token::Symbol')) {
		my $sym = $self->_handle_symbol($val);
		if ($sym->{atype} eq '$') {
			return ${ $sym->{ref} };
		}
		elsif ($sym->{atype} eq '@') {
			return $self->{ctx}=~/^scalar\b/
				? scalar(@{ ${ $sym->{ref} } })
				: @{ ${ $sym->{ref} } };
		}
		elsif ($sym->{atype} eq '%') {
			return $self->{ctx}=~/^scalar\b/
				? scalar(%{ ${ $sym->{ref} } })
				: %{ ${ $sym->{ref} } };
		}
		else { confess "bad symbol $sym" }
	}
	elsif ($val->isa('PPI::Token::Word') && $val->literal eq 'do'
		&& $val->snext_sibling && $val->snext_sibling->isa('PPI::Structure::Block'))
		{ return $self->_handle_block($val->snext_sibling) }
	elsif ($val->isa('PPI::Structure::List')) {
		my @l = do {
				# temporarily force list context to make _handle_list happy
				local $self->{ctx} = 'list';
				$self->_handle_list($val);
			};
		return $self->{ctx}=~/^scalar\b/ ? $l[-1] : @l;
	}
	elsif ($val->isa('PPI::Token::QuoteLike::Words')) { # qw//
		my @l = $val->literal; # returns a list of words
		return $self->{ctx}=~/^scalar\b/ ? $l[-1] : @l;
	}
	croak "Can't handle value "._errmsg($val);
}

# returns a hashref representing the symbol (see code below for details)
sub _handle_symbol {
	my ($self,$sym) = @_;
	confess "bad symbol" unless $sym->isa('PPI::Token::Symbol');
	my $peek_next = $sym->snext_sibling;
	my %rsym = ( name => $sym->symbol, atype => $sym->raw_type );
	if ($peek_next && $peek_next->isa('PPI::Structure::Subscript')) {
		my $sub = $self->_handle_subscript($peek_next);
		if ($sym->raw_type eq '$' && $sym->symbol_type eq '@' && $peek_next->braces eq '[]') {
			$rsym{ref} = \( $self->{out}{$sym->symbol}[$sub] );
			$rsym{sub} = "[$sub]";
		}
		elsif ($sym->raw_type eq '$' && $sym->symbol_type eq '%' && $peek_next->braces eq '{}') {
			$rsym{ref} = \( $self->{out}{$sym->symbol}{$sub} );
			$rsym{sub} = "{$sub}";
		}
		else { croak "Can't handle this subscript on this variable: "._errmsg($sym)._errmsg($peek_next) }
	}
	else {
		$rsym{ref} = \( $self->{out}{$sym->symbol} );
	}
	return \%rsym;
}

# returns a single value
sub _handle_subscript {
	my ($self,$sub) = @_;
	confess "bad subscript" unless $sub->isa('PPI::Structure::Subscript');
	my @sub_ch = $sub->schildren;
	croak "Expected subscript to contain a single expression\n"._errmsg($sub)
		unless @sub_ch==1 && $sub_ch[0]->isa('PPI::Statement::Expression');
	my @subs = $sub_ch[0]->schildren;
	croak "Expected subscript to contain a single value\n"._errmsg($sub)
		unless @subs==1;
	# autoquoting in hash braces
	if ($sub->braces eq '{}' && $subs[0]->isa('PPI::Token::Word'))
		{ return $subs[0]->literal }
	else {
		local $self->{ctx} = 'scalar';
		return $self->_handle_value($subs[0]);
	}
}

# returns arrayref or hashref
sub _handle_struct {
	my ($self,$constr) = @_;
	confess "bad struct class ".$constr->class
		unless $constr->isa('PPI::Structure::Constructor');
	local $self->{ctx} = 'list';
	if ($constr->braces eq '[]')
		{ return [$self->_handle_list($constr)] }
	elsif ($constr->braces eq '{}')
		{ return {$self->_handle_list($constr)} }
	croak "Unsupported constructor\n"._errmsg($constr);  # uncoverable statement
}

# handles the known PPI::Token::Quote subclasses
# returns a single value
sub _handle_quote {
	my ($self,$q) = @_;
	if ( $q->isa('PPI::Token::Quote::Single') || $q->isa('PPI::Token::Quote::Literal') )
		{ return $q->literal }
	elsif ( $q->isa('PPI::Token::Quote::Double') || $q->isa('PPI::Token::Quote::Interpolate') )
		{ return $self->_handle_interpolate($q) }
	confess "unknown PPI::Token::Quote subclass ".$q->class;  # uncoverable statement
}
# for use in _handle_quote; does very limited string interpolation
# returns a single value
sub _handle_interpolate {
	my ($self,$q) = @_;
	my $str = $q->string;
	# Perl (at least v5.20) doesn't allow trailing $, it does allow trailing @
	croak "Final \$ should be \\\$ or \$name" if $str=~/\$$/;
	# Variables
	$str=~s{(?<!\\)((?:\\\\)*)(\$\w+)}{$1.$self->_fetch_interp_var($2)}eg;
	$str=~s{(?<!\\)((?:\\\\)*)(\$)\{(\w+)\}}{$1.$self->_fetch_interp_var($2.$3)}eg;
	croak "Don't support string interpolation of '$1' in '$str' at "._errmsg($q)
		if $str=~/(?<!\\)(?:\\\\)*([\$\@].+)/;
	# Backslash escape sequences
	$str=~s{\\([0-7]{1,3}|x[0-9A-Fa-f]{2}|.)}{$self->_backsl($1)}eg;
	return $str;
}
my %_backsl_tbl = ( '\\'=>'\\', '$'=>'$', '"'=>'"', "'"=>"'", 'n'=>"\n", 'r'=>"\r", 't'=>"\t" );
sub _backsl { # for use in _handle_interpolate ONLY
	my ($self,$what) = @_;
	return chr(oct($what)) if $what=~/^[0-7]+$/;
	return chr(hex($1)) if $what=~/^x([0-9A-Fa-f]+)$/;  ## no critic (ProhibitCaptureWithoutTest)
	return $_backsl_tbl{$what} if exists $_backsl_tbl{$what};
	croak "Don't support escape sequence \"\\$what\"";
}
sub _fetch_interp_var { # for use in _handle_interpolate ONLY
	my ($self,$var) = @_;
	return $self->{out}{$var}
		if exists $self->{out}{$var} && defined $self->{out}{$var};
	warnings::warnif("Use of uninitialized value $var in interpolation");
	return "";
}


1;
