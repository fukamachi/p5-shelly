package App::clrun::impl;

use strict;
use warnings;

use Config::ENV 'LISP_IMPL';

my $config = $ENV{HOME} . '/.clrunrc';
my $implementations = -e $config ? load($config) : {};

common + {
    quit         => '(quit)',
    print_result => 1,
};

config ccl => +{
    impl_name     => 'ccl',
    eval          => '--eval',
    other_options => '--quiet --batch',
    binary        => $implementations->{ccl},
};

config sbcl => +{
    impl_name     => 'sbcl',
    pre_options   => '--noinform --disable-debugger',
    eval          => '--eval',
    other_options => '--quiet --batch',
    binary        => $implementations->{sbcl},
};

config alisp => +{
    impl_name     => 'alisp',
    eval          => '-e',
    quit          => '(exit 0 :quiet t)',
    other_options => '-batch',
    binary        => $implementations->{alisp},
};

config clisp => +{
    impl_name     => 'clisp',
    eval          => '-x',
    other_options => '-q --quiet',
    binary        => $implementations->{clisp},
    print_result  => 0,
};

config cmucl => +{
    impl_name     => 'cmucl',
    eval          => '-eval',
    other_options => '-quiet -batch',
    binary        => $implementations->{cmucl},
};

config ecl => +{
    impl_name     => 'ecl',
    eval          => '-eval',
    other_options => '-q',
    binary        => $implementations->{ecl},
};

1;
