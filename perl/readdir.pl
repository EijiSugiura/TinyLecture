#!/usr/bin/perl

use strict;

opendir(my $DIR,".");
foreach my $filename ( readdir($DIR) ){
    print "$filename\n";
}
closedir($DIR);
exit(0);
