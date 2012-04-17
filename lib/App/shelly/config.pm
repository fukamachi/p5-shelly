package App::shelly::config;

use strict;
use warnings;
use Exporter::Lite;

our @EXPORT_OK = qw(config config_path dumped_core_path);

my $local_base_path = $ENV{HOME} . '/.shelly/';
my $config;

sub local_path {
    return $local_base_path . $_[0];
}

sub config_path {
    return local_path('config');
}

sub dumped_core_path {
    return local_path('dumped-cores/')
      . ( $_[0] || ( $ENV{LISP_IMPL} . '.core' ) );
}

sub config {
    return $config if $config;

    my $config_file = config_path;

    $config =
      -e $config_file
      ? do $config_file
      : { implementations => {} };

    return $config;
}

1;
