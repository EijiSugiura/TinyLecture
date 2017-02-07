#!/usr/bin/perl

use strict;

opendir(my $DIR,".");
my @files = readdir($DIR);
closedir($DIR);

@files = grep(/.*\.csv$/,@files);
@files = sort(@files);

foreach my $filename ( @files ){
    my $counter = 0;
    open(my $FILE,$filename);
    while(<$FILE>){
	++$counter;
    }
    close($FILE);
    print "$filename,$counter\n";
}

exit(0);
