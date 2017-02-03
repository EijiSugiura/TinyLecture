#!/usr/bin/perl

use strict;

opendir(my $DIR,".");
foreach my $filename ( sort( grep(/.*\.csv$/, readdir($DIR) ) ) ){
    my $counter = 0;
    open(my $FILE,$filename);
    while(<$FILE>){
	++$counter;
    }
    close($FILE);
    print "$filename,$counter\n";
}
closedir($DIR);
exit(0);
