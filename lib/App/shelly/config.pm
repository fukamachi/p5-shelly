package App::shelly::config;

use strict;
use warnings;
use Exporter::Lite;

our @EXPORT_OK = qw(config);

my $config_file = $ENV{HOME} . '/.shelly/config';
my $config;

sub config {
    return $config if $config;

    $config =
      -e $config_file
      ? do $config_file
      : { implementations => {} };

    return $config;
}

1;
