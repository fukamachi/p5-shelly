package App::shelly::command;

use strict;
use warnings;

use App::shelly::impl;
use App::shelly::config qw(config shelly_path);

sub impl {
    sub { App::shelly::impl->param(@_); }
}

sub new {
    my ($class, %args) = @_;

    return bless {
        lisp_bin => $args{lisp_bin} || $ENV{LISP_BINARY},
        options  => [],
        core     => undef,
    }, $class;
}

sub add_option {
    my $self = shift;
    push @{ $self->{options} }, @_;
}

sub add_eval_option {
    my ($self, $command) = @_;
    $self->add_option(impl->('eval'), "'$command'");
}

sub set_core {
    my ($self, $core) = @_;
    $self->{core} = $core;
}

sub requires_quicklisp {
    $_[0]->add_eval_option(<<END_OF_LISP);
#-quicklisp (format *error-output* "~&[error] Shelly requires Quicklisp.~%") #+quicklisp t
END_OF_LISP
}

sub load_shelly {
    my ($self) = @_;

    if (my $shelly_path = shelly_path) {
        $shelly_path =~ s!/?$!/!;
        $self->add_eval_option("(require (quote asdf))");
        $self->add_eval_option(qq'(setf asdf:*central-registry* (cons #P"$shelly_path" asdf:*central-registry*))');
    }

    $self->add_eval_option(<<END_OF_LISP);
(let ((*standard-output* (make-broadcast-stream)))
  (handler-case #+quicklisp (ql:quickload :shelly) #-quicklisp (asdf:load-system :shelly)
    (#+quicklisp ql::system-not-found #-quicklisp asdf:missing-component (c)
     (format *error-output* "~&[error] ~A~&" c)
     #+quicklisp
     (format *error-output* "~&Try (ql:update-all-dists) to ensure your dist is up to date.~%")
     #+allegro (exit 1 :quiet t)
     #+sbcl    (sb-ext:exit)
     #-(or allegro sbcl) (quit)))
  (values))
END_OF_LISP
    $self->add_eval_option('(shelly.util::shadowing-use-package :shelly)');
}

sub check_shelly_version {
    my ($self) = @_;
    if (config->{version}) {
        $self->add_eval_option(qq((shelly.util::check-version "@{[ config->{version} ]}")));
    }
}

sub load_libraries {
    my ($self, $libraries) = @_;

    for ( @$libraries ) {
        $self->add_eval_option("(shelly.util::load-systems :$_)");
    }
}

sub load_shlyfile {
    my ($self, $shlyfile) = @_;

    if ($shlyfile && -f $shlyfile) {
        $self->add_eval_option(qq{(shelly.util::load-shlyfile #P"$shlyfile")});
    }
}

sub quit_lisp {
    $_[0]->add_eval_option('(swank-backend:quit-lisp)');
}

sub load_quicklisp {
    my ($self) = @_;
    if (config->{quicklisp_home}) {
        $self->add_eval_option(<<END_OF_LISP);
(let ((quicklisp-init (merge-pathnames "@{[ config->{quicklisp_home} ]}setup.lisp")))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))
END_OF_LISP
    }
}

sub run_shelly_command {
    my ($self, $args, $verbose) = @_;

    my @args = @$args;
    s/^\'/'\\''/ for @args;
    my $eval_expr =
        sprintf '(shelly.core::interpret (list %s) :verbose %s)',
            ( join " ", ( map { "\"$_\"" } @args ) ),
                $verbose ? 't' : 'nil';
    $self->add_eval_option($eval_expr);
    $self->quit_lisp;
}

sub run_repl {
    my ($self, $verbose) = @_;
    $self->add_eval_option(
        sprintf '(shelly::run-repl :verbose %s)', $verbose ? 't' : 'nil'
    );
}

sub stringify {
    my ($self) = @_;

    return join ' ', $self->{lisp_bin},
        ($self->{core} ? (impl->('core_option'), $self->{core}) : ()),
        (impl->('pre_options') || ()), @{ $self->{options} }, (impl->('other_options') || ());
}

1;
