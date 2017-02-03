#!/usr/bin/perl

use strict;

opendir(my $DIR,".");

my @files = readdir($DIR);

print "$files[0]\n";
print "$files[1]\n";

closedir($DIR);
exit(0);
