use strict;
use warnings;

use FindBin qw($Bin);
use lib "$Bin/../lib", "$Bin/../fatlib";
use App::shelly;

unless (caller) {
    my $app = App::shelly->new;
    $app->parse_options(@ARGV);
    $app->doit or exit(1);
}
