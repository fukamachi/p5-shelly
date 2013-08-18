package App::shelly;

use strict;
use warnings;

use Getopt::Long qw(:config gnu_getopt pass_through);
use File::Which qw(which);

use App::shelly::impl;
use App::shelly::config qw(config dumped_core_path shelly_path);

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
        'help|h'   => \$self->{help},
        'impl|I=s' => \$self->{lisp_impl},
        'load|L=s' => \my $libraries,
        'file|f=s' => \$self->{shlyfile},
        'no-init'  => \$self->{noinit},
        'verbose'  => \$self->{verbose},
        'debug'    => \$self->{debug},
    );

    if ($libraries) {
        $self->{load_libraries} = [ split ',', $libraries ];
    }

    $self->{argv} = \@ARGV;

    if ( $self->{help} ) {
        $self->{noinit} = 1;
        $self->{argv}   = ['shelly::help'];
    }
}

sub doit {
    my ($self) = @_;

    unless ( $self->{lisp_impl} ) {
        $self->{lisp_impl} = $self->detect_installed_lisp;
    }

    local $ENV{LISP_IMPL} = $self->{lisp_impl};

    unless ( impl->('impl_name') ) {
        print "Unsupported CL implementation: @{[ $self->{lisp_impl} ]}\n";
        print qq(It must be one of "sbcl", "ccl", "alisp", "clisp", "cmucl" or "ecl".\n);
        exit 1;
    }

    my $command = $self->_build_command;

    if ( $self->{debug} ) {
        print $command, "\n";
    }

    system(qq($command 2>&1));
}

sub _build_command {
    my ($self) = @_;

    my $lisp_bin = $ENV{LISP_BINARY} || impl->('binary') || $self->{lisp_impl};

    $ENV{LISP_BINARY} = $lisp_bin;

    if ( $self->{noinit} ) {
        # FIXME: Need to load Quicklisp even when `--noinit' is specified.
        $lisp_bin .= ' ' . impl->('noinit_option');
    }

    my @args = @{ $self->{argv} };
    my @evals = ();

    if ( -e dumped_core_path ) {
        $lisp_bin = join ' ',
          ( $lisp_bin, impl->('core_option'), dumped_core_path );
    }
    else {
        unless ($self->{lisp_impl} eq 'ecl' || $args[0] eq 'install') {
            print STDERR
                "Warning: Core image wasn't found. It is probably slow, isn't it? Try \"shly dump-core\".\n";
        }

        push @evals, <<END_OF_LISP;
#-quicklisp (format *error-output* "~&[error] Shelly requires Quicklisp but it isn't loaded.~%")
END_OF_LISP

        if (my $shelly_path = shelly_path) {
            push @evals, "(require (quote asdf))";
            push @evals, qq'(setf asdf:*central-registry* (cons #P"$shelly_path" asdf:*central-registry*))';
        }

        push @evals, <<END_OF_LISP;
(let ((*standard-output* (make-broadcast-stream)))
  (handler-case #+quicklisp (ql:quickload :shelly) #-quicklisp (asdf:load-system :shelly)
    (#+quicklisp ql::system-not-found #-quicklisp asdf:missing-component (c)
     (format *error-output* "~&[error] ~A~&" c)
     #+quicklisp
     (format *error-output* "~&Try (ql:update-all-dists) to ensure your dist is up to date.~%")
     #+allegro (exit 1 :quiet t)
     #-allegro (quit)))
  (values))
END_OF_LISP
        push @evals, '(shelly.util::shadowing-use-package :shelly)';
    }

    if ($args[0] ne 'install' && config->{version}) {
        push @evals,
          qq((shelly.util::check-version "@{[ config->{version} ]}"));
    }

    for ( @{ $self->{load_libraries} } ) {
        push @evals, "(shelly.util::load-systems :$_)";
    }

    my $shlyfile = exists $self->{shlyfile} ? $self->{shlyfile} : 'shlyfile';
    if ($shlyfile && -f $shlyfile) {
        push @evals, qq{(shelly.util::load-shlyfile #P"$shlyfile")};
    }

    {
        if ( @args > 0 ) {
            s/^\'/'\\''/ for @args;
            my $eval_expr =
              sprintf '(shelly.core::interpret (list %s) :verbose %s)',
              ( join " ", ( map { "\"$_\"" } @args ) ),
              $self->{verbose} ? 't' : 'nil';
            push @evals, $eval_expr;
            push @evals, '(swank-backend:quit-lisp)';
        }
        else {
            push @evals, sprintf '(shelly::run-repl :verbose %s)',
              $self->{verbose} ? 't' : 'nil';
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

sub detect_installed_lisp {
    print "LISP_IMPL isn't set. Auto detecting...\n";

    my (@lisp_impl) =
      grep { which($_) } qw(sbcl ccl alisp clisp cmucl lisp ecl);
    @lisp_impl = map { $_ eq 'lisp' ? 'cmucl' : $_ } @lisp_impl;

    unless (@lisp_impl) {
        print "Couldn't detect installed Lisp.\n";
        exit 1;
    }

    print "Installed Lisp: " . ( join ', ', @lisp_impl ) . "\n";

    if ( @lisp_impl > 1 ) {
        print "Which do you prefer? [@{[ $lisp_impl[0] ]}] : ";

        my $input = <STDIN>;

        {
            no warnings 'uninitialized';
            chomp $input;
        }

        unless ($input) {
            $input = $lisp_impl[0];
            print $input, "\n";
        }

        return $input;
    }

    return $lisp_impl[0];
}

1;

__END__

=head1 NAME

App::shelly

=head1 SYNOPSIS

$ shly [options] [atom...]

=head1 OPTIONS

=over 4

=item B<-h, --help>

Show this help.

=item B<-I, --impl [implementation]>

Tell what Lisp implementation to use. The default is $LISP_IMPL.

=item B<-L, --load [library1,library2,...]>

Load libraries before executing the expression.

=item B<--verbose>

Print some informations.

=item B<--no-init>

Do not load the user init file.

=item B<--debug>

This flag is for Shelly developers.

=back

=cut
