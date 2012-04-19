package App::shelly;

use strict;
use warnings;

use Getopt::Long qw(:config gnu_getopt pass_through);
use File::Which qw(which);

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
        'help|h'   => \$self->{help},
        'impl|I=s' => \$self->{lisp_impl},
        'load|L=s' => \my $libraries,
        'install'  => \$self->{install},
        'debug'    => \$self->{debug},
    );

    if ($libraries) {
        $self->{load_libraries} = [ split ',', $libraries ];
    }

    $self->{argv} = \@ARGV;
}

sub doit {
    my ($self) = @_;

    unless ( $self->{lisp_impl} ) {
        $self->{lisp_impl} = $self->detect_installed_lisp;
    }

    local $ENV{LISP_IMPL} = $self->{lisp_impl};

    unless ( impl->('impl_name') ) {
        print "Unsupported CL implementation: @{[ $self->{lisp_impl} ]}\n";
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

    my $lisp_bin = impl->('binary') || $self->{lisp_impl};

    if ( -e dumped_core_path ) {
        $lisp_bin = join ' ',
          ( $lisp_bin, impl->('core_option'), dumped_core_path );
    }
    else {
        if ( $self->{lisp_impl} ne 'ecl' && !$self->{install} ) {
            print
"Warning: Core image wasn't found. It is probably slow, isn't it? Try \"shly shelly:dump-core\".\n";
        }
    }

    my @evals = ();
    if ( $self->{install} || !-e dumped_core_path ) {
        push @evals, <<END_OF_LISP;
(let ((*standard-output* (make-broadcast-stream)))
  (handler-case (ql:quickload :shelly)
    (quicklisp-client::system-not-found ()
     (format *error-output* "Shelly is not found. Try (ql:update-all-dists) to ensure your dist is up to date.~%")
     #+allegro (exit 1 :quiet t)
     #-allegro (quit)))
  (values))
END_OF_LISP
    }

    if ( config->{version} ) {
        push @evals, qq((shelly:check-version "@{[ config->{version} ]}"));
    }

    if ( $self->{install} ) {
        push @evals, '(shelly:install-script :quit-lisp t)';
    }

    if ( @{ $self->{load_libraries} } ) {
        my $load_libraries = sprintf q((quote (%s))), join ' ',
          ( map { ":$_" } @{ $self->{load_libraries} } );

        my $eval_libs = sprintf q((shelly:load-libraries %s)),
          $load_libraries;
        push @evals, $eval_libs;
    }

    {
        my @args = @{ $self->{argv} };

        if ( @args > 1 ) {
            my $eval_expr = sprintf '(shelly:interpret %s)',
              ( join " ", ( map { "\"$_\"" } @args ) );
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

=item B<--install>

Install Shelly into ~/.shelly/.

=item B<-I, --impl [implementation]>

Tell what Lisp implementation to use. The default is $LISP_IMPL.

=item B<-L, --load>

Load libraries before executing the expression.

=item B<--debug>

This flag is for Shelly developers.

=back

=cut
