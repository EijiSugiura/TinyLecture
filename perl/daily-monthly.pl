#!/usr/bin/perl

use strict;

my $MONTH = $ARGV[0];

my %counter;

opendir(my $DIR,".");
#foreach my $filename ( sort( grep(/.*$MONTH.*\.csv\.xz$/, readdir($DIR) ) ) ){
#    open(my $FILE,"xz -dc $filename |");
foreach my $filename ( sort( grep(/.*$MONTH.*\.csv$/, readdir($DIR) ) ) ){
    open(my $FILE,$filename);
    while(my $line = <$FILE>) {
	my $name = (split(/,/, $line))[6];
	++$counter{$name};
    }
    close($FILE);
}
closedir($DIR);

print "$MONTH\n";
my @names = sort {$counter{$b} <=> $counter{$a}} keys(%counter);
foreach my $name (splice(@names, 0, 10)) {
    print "$name,$counter{$name}\n";
}
exit(0);
