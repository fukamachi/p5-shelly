package App::clrun;

use strict;
use warnings;

use Getopt::Long;

sub new {
    my ($class) = @_;

    return bless { argv => [] }, $class;
}

sub parse_options {
    my ( $self, @argv ) = @_;

    local @ARGV = @{ $self->{argv} };
    push @ARGV, @_;

    $self->{argv} = \@ARGV;
}

sub doit {
    my ($self) = @_;
}

1;
