package App::clrun;

use strict;
use warnings;

use Getopt::Long qw(:config gnu_getopt pass_through);

use App::clrun::impl;
use App::clrun::type;

sub impl {
    sub { App::clrun::impl->param(@_); }
}

sub new {
    my ($class) = @_;

    return bless {
        lisp_impl      => $ENV{LISP_IMPL},
        load_libraries => [],
        argv           => [],
    }, $class;
}

sub parse_options {
    my ( $self, @argv ) = @_;

    local @ARGV = @{ $self->{argv} };
    push @ARGV, @argv;

    GetOptions(
        'impl|I=s'  => \my $lisp_impl,
        'load|L=s'  => \my $libraries,
        'verbose|v' => \my $verbose,
    );

    $self->{verbose} = $verbose;

    if ($lisp_impl) {
        $self->{lisp_impl} = $lisp_impl;
    }

    if ($libraries) {
        $self->{load_libraries} = [ split ',', $libraries ];
    }

    $self->{argv} = \@ARGV;
}

sub doit {
    my ($self) = @_;

    local $ENV{LISP_IMPL} = $self->{lisp_impl};

    unless ( $self->{lisp_impl} ) {
        die 'LISP_IMPL must be set.';
    }

    unless ( impl->('impl_name') ) {
        die 'Unsupported CL implementation: ' . $self->{lisp_impl};
    }

    my $command = $self->_build_command;

    if ( $self->{verbose} ) {
        print $command, "\n";
    }

    system(qq($command 2>&1));
}

sub _build_command {
    my ($self) = @_;

    my $lisp_bin = impl->('binary') || $self->{lisp_impl};

    my $eval_libs = '';
    if ( @{ $self->{load_libraries} } ) {
        $eval_libs = sprintf q('(ql:quickload (quote (%s)))'), join ' ',
          ( map { ":$_" } @{ $self->{load_libraries} } );
    }
    my $eval_expr = '';
    my $eval_quit = '';
    {
        my @args = @{ $self->{argv} };
        my $fn   = shift @args;
        if ( defined $fn ) {
            $_ = canonicalize_arg($_) for @args;
            if ( impl->('print_result') ) {
                $eval_expr = sprintf "'(ignore-errors (print (%s)))'",
                  ( join ' ', $fn, @args );
            }
            else {
                $eval_expr = "'(" . ( join ' ', $fn, @args ) . ")'";
            }
            $eval_quit = "'" . impl->('quit') . "'";
        }
    }

    my $command = join ' ', $lisp_bin,
      (
          impl->('pre_options')
        ? impl->('pre_options')
        : ()
      ),
      ( $eval_libs ? ( impl->('eval'), $eval_libs ) : () ),
      ( $eval_expr ? ( impl->('eval'), $eval_expr ) : () ),
      ( $eval_quit ? ( impl->('eval'), $eval_quit ) : () ),
      impl->('other_options');

    return $command;
}

sub canonicalize_arg {
    my ($arg) = @_;

    if ( $arg =~ /^--/ ) {
        $arg =~ s/^--/:/;
    }
    elsif ( is_pathname($arg) ) {
        $arg = qq(\#p"$arg");
    }
    elsif ( is_string($arg) ) {
        $arg = qq("$arg");
    }

    return $arg;
}

1;
