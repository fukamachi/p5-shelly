package App::shelly;

use strict;
use warnings;

use Getopt::Long qw(:config gnu_getopt pass_through);

use App::shelly::impl;
use App::shelly::config qw(config dumped_core_path);

sub impl {
    sub { App::shelly::impl->param(@_); }
}

sub new {
    my ($class) = @_;

    return bless {
        lisp_impl => $ENV{LISP_IMPL} || config->{default_lisp},
        load_libraries => [],
        argv           => [],
    }, $class;
}

sub parse_options {
    my ( $self, @argv ) = @_;

    local @ARGV = @{ $self->{argv} };
    push @ARGV, @argv;

    GetOptions(
        'help|h'    => \$self->{help},
        'impl|I=s'  => \$self->{lisp_impl},
        'load|L=s'  => \my $libraries,
        'dump-core' => \$self->{dump_core},
        'rm-core'   => \$self->{rm_core},
        'debug'     => \$self->{debug},
    );

    if ($libraries) {
        $self->{load_libraries} = [ split ',', $libraries ];
    }

    $self->{argv} = \@ARGV;
}

sub doit {
    my ($self) = @_;

    local $ENV{LISP_IMPL} = $self->{lisp_impl};

    unless ( $self->{lisp_impl} ) {
        print "LISP_IMPL must be set.\n";
        exit 1;
    }

    unless ( impl->('impl_name') ) {
        print "Unsupported CL implementation: @{[ $self->{lisp_impl} ]}\n";
        exit 1;
    }

    if ( $self->{rm_core} ) {
        unless ( -e dumped_core_path ) {
            print "Core file doesn't exist: @{[ dumped_core_path ]}\n";
            exit 1;
        }

        system(qq(rm @{[ dumped_core_path ]}));

        print "Successfully deleted: @{[ dumped_core_path ]}\n";
        exit;
    }

    my $command = $self->_build_command;

    if ( $self->{debug} ) {
        print $command, "\n";
    }

    system(qq($command 2>&1));
}

sub _build_command {
    my ($self) = @_;

    my $lisp_bin = impl->('binary') || $self->{lisp_impl};

    if ( -e dumped_core_path ) {
        $lisp_bin = join ' ',
          ( $lisp_bin, impl->('core_option'), dumped_core_path );
    }

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

    if ( $self->{dump_core} ) {
        push @evals, sprintf '(shelly:dump-core "%s")', dumped_core_path;
    }

    {
        my @args = @{ $self->{argv} };
        my $fn   = shift @args;

        if ( defined $fn ) {
            my $eval_expr = sprintf '(shelly:interpret "%s")',
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

__END__

=head1 NAME

App::shelly

=head1 SYNOPSIS

$ shly [options] [atom...]

Options: -h, -I, -L, --debug

=head1 OPTIONS

=over 4

=item B<-h, --help>

Show this help.

=item B<-I, --impl [implementation]>

Tell what Lisp implementation to use. The default is $LISP_IMPL.

=item B<-L, --load>

Load libraries before executing the expression.

=item B<--debug>

This flag is for Shelly developers.

=back

=cut
