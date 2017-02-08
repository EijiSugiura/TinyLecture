#!/usr/bin/perl

use strict;

opendir(my $DIR,".");
foreach my $filename ( sort( grep(/.*\.csv$/, readdir($DIR) ) ) ){
    my %counter;
    open(my $FILE,$filename);
    while(my $line = <$FILE>) {
	my ($name,$rev) = (split(/,/, $line))[6,9];
	++$counter{$name.','.$rev};
    }
    close($FILE);
    print "$filename\n";
    my @names = sort {$counter{$b} <=> $counter{$a}} keys(%counter);
    foreach my $name (splice(@names, 0, 10)) {
	print "$name,$counter{$name}\n";
    }
}
closedir($DIR);
exit(0);
