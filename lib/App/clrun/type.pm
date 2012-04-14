package App::clrun::type;

use strict;
use warnings;

use Exporter::Lite;

our @EXPORT =
qw(is_number is_boolean is_pathname is_keyword is_list is_dispatch_macro is_quoted is_string);

sub is_number  { $_[0] =~ /^[-+]?\d+\.?\d*$/ }
sub is_boolean { $_[0] =~ /^(?:T|NIL)$/i }
sub is_pathname { is_string( $_[0] ) and -e $_[0] }
sub is_keyword        { $_[0] =~ /^:/ }
sub is_list           { $_[0] =~ /^\(.*\)$/ }
sub is_dispatch_macro { $_[0] =~ /^\#/ }
sub is_quoted         { $_[0] =~ /^\'/ }

sub is_string {
          !&is_number
      and !&is_boolean
      and !&is_keyword
      and !&is_quoted
      and !&is_dispatch_macro
      and !&is_list;
}

1;
