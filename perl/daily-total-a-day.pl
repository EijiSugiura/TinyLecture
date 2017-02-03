#!/usr/bin/perl

use strict;

my $filename = "ids-total-2016-01-01.csv";
my $counter = 0;
open(my $FILE,$filename);
while(<$FILE>){
    ++$counter;
}
close($FILE);
print "$filename,$counter\n";

exit(0);
