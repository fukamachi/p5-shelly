#!/usr/bin/env perl
use strict;
use warnings;

use FindBin qw($Bin);
use lib "$Bin/../lib";

open my $in,  '<', "$Bin/shly.PL"     or die $!;
open my $out, '>', "$Bin/../bin/shly" or die $!;

local $/ = undef;
my $startup = <$in>;
print $out '#!/usr/bin/env perl', "\n";
print $out scalar `$^X -e "use App::FatPacker -run_script" file`;
print $out $startup;
