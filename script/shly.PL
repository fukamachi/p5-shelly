use strict;
use warnings;

use FindBin qw($Bin);
use lib "$Bin/../lib", "$Bin/../fatlib";
use App::shelly;

use Pod::Usage;
use Pod::Find qw(pod_where);

unless (caller) {
    my $app = App::shelly->new;
    $app->parse_options(@ARGV);
    if ( $app->{help} ) {
        pod2usage(
            verbose => 1,
            -input  => pod_where( { -inc => 1 }, 'App::shelly' )
        );
    }
    $app->doit or exit(1);
}
