#!/usr/bin/env perl
use strict;
use warnings;

my %person = (
    name => 'John',
    age => 30,
    city => 'New York'
);

print "Person:\n";
foreach my $key (keys %person) {
    print "  $key: $person{$key}\n";
}

print "\nName: $person{name}\n";
print "Age: $person{age}\n";

$person{email} = 'john@example.com';
print "\nAfter adding email:\n";
foreach my $key (keys %person) {
    print "  $key: $person{$key}\n";
}

if (exists $person{name}) {
    print "\nName exists: $person{name}\n";
}

delete $person{email};
print "\nAfter deleting email:\n";
foreach my $key (keys %person) {
    print "  $key: $person{$key}\n";
}

my @keys = keys %person;
print "\nKeys: @keys\n";

my @values = values %person;
print "Values: @values\n";
