#!/usr/bin/env swift

var numbers = [1, 2, 3, 4, 5]

print("Numbers: \(numbers)")
print("Count: \(numbers.count)")
print("Sum: \(numbers.reduce(0, +))")
print("Max: \(numbers.max() ?? 0)")
print("Min: \(numbers.min() ?? 0)")

numbers.append(6)
print("After append: \(numbers)")

numbers.removeLast()
print("After removeLast: \(numbers)")

let squared = numbers.map { $0 * $0 }
print("Squared: \(squared)")

let evens = numbers.filter { $0 % 2 == 0 }
print("Even numbers: \(evens)")

let sorted = numbers.sorted(by: >)
print("Sorted descending: \(sorted)")

print("First element: \(numbers.first ?? 0)")
print("Last element: \(numbers.last ?? 0)")
