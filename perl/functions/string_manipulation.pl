#!/usr/bin/env perl
use strict;
use warnings;

my $text = "  Hello, World!  ";

print "Original: '$text'\n";
print "Upper: '" . uc($text) . "'\n";
print "Lower: '" . lc($text) . "'\n";

my $trimmed = $text;
$trimmed =~ s/^\s+|\s+$//g;
print "Trim: '$trimmed'\n";

my $replaced = $text;
$replaced =~ s/World/Perl/;
print "Replace: '$replaced'\n";

my @parts = split(/, /, $trimmed);
print "Split: @parts\n";

print "Contains 'World': " . ($text =~ /World/ ? "true" : "false") . "\n";
print "Length: " . length($text) . "\n";

my $substring = substr($trimmed, 0, 5);
print "Substring: '$substring'\n";

my $repeated = "-" x 20;
print "Repeat: '$repeated'\n";
