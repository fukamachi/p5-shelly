use strict;
use warnings;
use ExtUtils::MakeMaker;

WriteMakefile(
    NAME          => 'App::clrun',
    AUTHOR        => q{Eitarow Fukamachi <e.arrows@gmail.com>},
    VERSION_FROM  => 'lib/App/clrun.pm',
    ABSTRACT_FROM => 'lib/App/clrun.pm',
    ( $ExtUtils::MakeMaker::VERSION >= 6.3002
        ? ( 'LICENSE' => 'perl' )
        : () ),
    PL_FILES  => {},
    PREREQ_PM => {
        'Test::More' => 0,
    },
    dist  => { COMPRESS => 'gzip -9f', SUFFIX => 'gz', },
    clean => { FILES    => 'App-clrun-*' },
);
