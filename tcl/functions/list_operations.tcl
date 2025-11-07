#!/usr/bin/env tclsh
# List operations in TCL

puts "\n=== List Operations in TCL ===\n"

# Create lists
set numbers {1 2 3 4 5}
set fruits {apple banana cherry}

puts "Numbers: $numbers"
puts "Fruits: $fruits"

# List length
puts "\nLength: [llength $numbers]"

# Access elements
puts "First (index 0): [lindex $numbers 0]"
puts "Last (index end): [lindex $numbers end]"

# Append
lappend numbers 6
puts "\nAfter lappend 6: $numbers"

# Concatenation
set combined [concat $numbers {7 8 9}]
puts "After concat: $combined"

# Range
set range [list]
for {set i 1} {$i <= 10} {incr i} {
    lappend range $i
}
puts "\nRange 1-10: $range"

# Sort
set unsorted {5 2 8 1 9 3}
set sorted [lsort -integer $unsorted]
puts "\nSorted: $sorted"

# Reverse
set reversed [lreverse $numbers]
puts "Reversed: $reversed"

# Search
set idx [lsearch $numbers 3]
puts "\nSearch 3: index $idx"

# Map (using foreach)
set squared [list]
foreach num $numbers {
    lappend squared [expr {$num * $num}]
}
puts "\nSquared: $squared"

# Filter (evens)
set evens [list]
foreach num $numbers {
    if {[expr {$num % 2}] == 0} {
        lappend evens $num
    }
}
puts "Evens: $evens"

# Sum
set sum 0
foreach num $numbers {
    set sum [expr {$sum + $num}]
}
puts "\nSum: $sum"
