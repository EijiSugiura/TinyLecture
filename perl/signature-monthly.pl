#!/usr/bin/perl

use strict;

my $SIGNATURE = $ARGV[0];

my %counter;
print "$SIGNATURE\n";
opendir(my $DIR,".");
foreach my $filename ( sort( grep(/.*\.csv\.xz$/, readdir($DIR) ) ) ){
    open(my $FILE,"xz -dc $filename |");
#foreach my $filename ( sort( grep(/.*\.csv$/, readdir($DIR) ) ) ){
#    open(my $FILE,$filename);
    
    while(my $line = grep(/$SIGNATURE/,<$FILE>)) {
	my $date = (split(/,/, $line))[0];
	$date = (split(/\s/,$date))[0];
	++$counter{$date};
    }
    close($FILE);
    print "$date,$counter{$date}\n";
}
closedir($DIR);

exit(0);
