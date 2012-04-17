package App::shelly::config;

use strict;
use warnings;
use Exporter::Lite;

our @EXPORT_OK = qw(config config_path dumped_core_path);

my $local_base_path = $ENV{HOME} . '/.shelly/';

sub local_path {
    return $local_base_path . $_[0];
}

sub config_path {
    my ($impl) = @_;

    if ($impl) {
        return local_path( 'config.' . $impl );
    }

    return local_path('config');
}

sub dumped_core_path {
    return local_path('dumped-cores/')
      . ( $_[0] || ( $ENV{LISP_IMPL} . '.core' ) );
}

sub config {
    my $config_file = &config_path;

    my $config =
      -e $config_file
      ? do $config_file
      : {};

    return $config;
}

1;
