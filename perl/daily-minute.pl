#!/usr/bin/perl

use strict;

my $filename = "ids-total-2016-01-01.csv.xz";
my %counter;
open(my $FILE,"xz -dc $filename|");
while(my $line = <$FILE>){
    my $date = (split(/,/,$line))[0];
#    if( $date =~ /^(.* \d{2}:).*/) { # 1hour
#    if( $date =~ /^(.* \d{2}:\d).*/) {	# 10minute
    if( $date =~ /^(.* \d{2}:\d{2}).*/) {	# 1minute	
	$date = $1;
	++$counter{$date};
    }
}
close($FILE);
foreach my $date (sort keys %counter){
    print "$date,$counter{$date}\n";
}

exit(0);
