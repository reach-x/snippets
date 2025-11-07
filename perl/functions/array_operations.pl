#!/usr/bin/env perl
use strict;
use warnings;
use List::Util qw(sum max min);

my @numbers = (1, 2, 3, 4, 5);

print "Numbers: @numbers\n";
print "Size: " . scalar(@numbers) . "\n";
print "Sum: " . sum(@numbers) . "\n";
print "Max: " . max(@numbers) . "\n";
print "Min: " . min(@numbers) . "\n";

push(@numbers, 6);
print "After push: @numbers\n";

pop(@numbers);
print "After pop: @numbers\n";

my @squared = map { $_ * $_ } @numbers;
print "Squared: @squared\n";

my @evens = grep { $_ % 2 == 0 } @numbers;
print "Even numbers: @evens\n";

my @sorted = sort { $b <=> $a } @numbers;
print "Sorted descending: @sorted\n";

print "First element: $numbers[0]\n";
print "Last element: $numbers[-1]\n";
