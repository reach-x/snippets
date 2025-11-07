#!/usr/bin/env groovy
// List operations in Groovy

println "\n=== List Operations in Groovy ===\n"

// Create lists
def numbers = [1, 2, 3, 4, 5]
def fruits = ['apple', 'banana', 'cherry']

println "Numbers: $numbers"
println "Fruits: $fruits"

// List size
println "\nSize: ${numbers.size()}"

// Access elements
println "First: ${numbers.first()}"
println "Last: ${numbers.last()}"
println "Element at 2: ${numbers[2]}"

// Add elements
numbers << 6
println "\nAfter << 6: $numbers"

numbers += [7, 8, 9]
println "After += [7,8,9]: $numbers"

// Contains
println "\nContains 3: ${numbers.contains(3)}"
println "Contains 100: ${numbers.contains(100)}"

// Collect (map)
def squared = numbers.collect { it * it }
println "\nCollect (square): $squared"

// FindAll (filter)
def evens = numbers.findAll { it % 2 == 0 }
println "FindAll (even): $evens"

// Inject (reduce)
def sum = numbers.inject(0) { acc, val -> acc + val }
println "\nInject (sum): $sum"

// Any and every
println "\nAny > 5: ${numbers.any { it > 5 }}"
println "Every > 0: ${numbers.every { it > 0 }}"

// Sort
def unsorted = [5, 2, 8, 1, 9, 3]
println "\nSorted: ${unsorted.sort()}"
println "Sorted (reverse): ${unsorted.sort().reverse()}"

// Unique
def withDupes = [1, 2, 2, 3, 3, 3, 4]
println "\nUnique: ${withDupes.unique()}"

// Take and drop
println "\nTake 3: ${numbers.take(3)}"
println "Drop 2: ${numbers.drop(2)}"

// Ranges
println "\nRange 1..10: ${(1..10).toList()}"

// Closure
def doubler = { x -> x * 2 }
println "Closure (double 5): ${doubler(5)}"
