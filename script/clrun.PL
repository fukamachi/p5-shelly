use strict;
use warnings;

use FindBin qw($Bin);
use lib "$Bin/../lib", "$Bin/../fatlib";
use App::clrun;

unless (caller) {
    my $app = App::clrun->new;
    $app->parse_options(@ARGV);
    $app->doit or exit(1);
}
