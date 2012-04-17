package App::shelly::impl;

use strict;
use warnings;

use Config::ENV 'LISP_IMPL';

use App::shelly::config;

my $impl = App::shelly::config::config->{implementations};

common + {
    print_result => 1,
};

config ccl => +{
    impl_name     => 'ccl',
    eval          => '--eval',
    other_options => '--quiet --batch',
    binary        => $impl->{ccl},
    core_option   => '-I',
};

config sbcl => +{
    impl_name     => 'sbcl',
    pre_options   => '--noinform --disable-debugger',
    eval          => '--eval',
    other_options => '--quiet --batch',
    binary        => $impl->{sbcl},
    core_option   => '--core',
};

config alisp => +{
    impl_name     => 'alisp',
    pre_options   => '-L ~/.clinit.cl',
    eval          => '-e',
    other_options => '-batch',
    binary        => $impl->{alisp},
    core_option   => '-I',
};

config clisp => +{
    impl_name     => 'clisp',
    eval          => '-x',
    other_options => '-q --quiet',
    binary        => $impl->{clisp},
    print_result  => 0,
    core_option   => '-M',
};

config cmucl => +{
    impl_name     => 'cmucl',
    eval          => '-eval',
    other_options => '-quiet -batch',
    binary        => $impl->{cmucl} || 'lisp',
    core_option   => '-core',
};

config ecl => +{
    impl_name     => 'ecl',
    eval          => '-eval',
    other_options => '-q',
    binary        => $impl->{ecl},
};

1;
