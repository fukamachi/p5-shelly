#!perl -T

use Test::More tests => 1;

BEGIN {
    use_ok( 'App::clrun' ) || print "Bail out!\n";
}

diag( "Testing App::clrun $App::clrun::VERSION, Perl $], $^X" );
