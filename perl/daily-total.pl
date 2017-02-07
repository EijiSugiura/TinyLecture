#!/usr/bin/perl

use strict;

opendir(my $DIR,".");
foreach my $filename ( sort( grep(/.+\.csv$/, readdir($DIR) ) ) ){
    open(my $FILE,$filename);
    my $counter = 0;
    while(<$FILE>){
	++$counter;
    }
    print "$filename,$counter\n";
    close($FILE);
}
closedir($DIR);
exit(0);
