package App::shelly::impl;

use strict;
use warnings;

use Config::ENV 'LISP_IMPL';

use App::shelly::config;

common + {};

config ccl => +{
    impl_name     => 'ccl',
    eval          => '--eval',
    other_options => '--quiet',
    binary        => App::shelly::config::config('ccl')->{binary_path},
    core_option   => '-I',
};

config sbcl => +{
    impl_name     => 'sbcl',
    pre_options   => '--noinform --disable-debugger',
    eval          => '--eval',
    other_options => '--quiet',
    binary        => App::shelly::config::config('sbcl')->{binary_path},
    core_option   => '--core',
};

config alisp => +{
    impl_name     => 'alisp',
    pre_options   => '-L ~/.clinit.cl',
    eval          => '-e',
    other_options => '-batch',
    binary        => App::shelly::config::config('alisp')->{binary_path},
    core_option   => '-I',
};

config clisp => +{
    impl_name     => 'clisp',
    eval          => '-x',
    other_options => '-q --quiet',
    binary        => App::shelly::config::config('clisp')->{binary_path},
    core_option   => '-M',
};

config cmucl => +{
    impl_name     => 'cmucl',
    eval          => '-eval',
    other_options => '-quiet -batch',
    binary => App::shelly::config::config('cmucl')->{binary_path} || 'lisp',
    core_option => '-core',
};

config ecl => +{
    impl_name     => 'ecl',
    eval          => '-eval',
    other_options => '-q',
    binary        => App::shelly::config::config('ecl')->{binary_path},
};

1;
