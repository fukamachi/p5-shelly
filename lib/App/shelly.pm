package App::shelly;

use strict;
use warnings;

use Getopt::Long qw(:config gnu_getopt pass_through);

use App::shelly::impl;

sub impl {
    sub { App::shelly::impl->param(@_); }
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

    my @evals = (<<END_OF_LISP);
(let ((*standard-output* (make-broadcast-stream)))
  (ql:quickload :shelly)
  (values))
END_OF_LISP

    if ( @{ $self->{load_libraries} } ) {
        my $eval_libs = sprintf q((ql:quickload (quote (%s)))), join ' ',
          ( map { ":$_" } @{ $self->{load_libraries} } );

        push @evals, $eval_libs;
    }

    {
        my @args = @{ $self->{argv} };
        my $fn   = shift @args;

        if ( defined $fn ) {
            my $eval_expr = sprintf '(shelly:shelly-interpret "%s")',
              ( join ' ', $fn, @args );
            push @evals, $eval_expr;
            push @evals, '(swank-backend:quit-lisp)';
        }
        else {
            push @evals, '(shelly:run-repl)';
        }
    }

    my $command = join ' ', $lisp_bin,
      (
          impl->('pre_options')
        ? impl->('pre_options')
        : ()
      ),
      ( map { ( impl->('eval'), "'$_'" ) } @evals ),
      impl->('other_options');

    return $command;
}

1;
