#!/usr/bin/perl
use strict;
use Data::Dumper;

my @array;

print "1.\n";
push(@array,"Hello");
print Dumper \@array;

print "2.\n";
push(@array,"World");
print Dumper \@array;

print "3.\n";
pop(@array);
print Dumper \@array;

print "4.\n";
push(@array,"World");
shift(@array);
print Dumper \@array;

exit(0);
