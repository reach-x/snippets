# Sequence operations in Nim

import sequtils, algorithm, strutils

echo "\n=== Sequence Operations in Nim ===\n"

# Create sequences
var numbers = @[1, 2, 3, 4, 5]
var fruits = @["apple", "banana", "cherry"]

echo "Numbers: ", numbers
echo "Fruits: ", fruits

# Sequence length
echo "\nLength: ", len(numbers)

# Access elements
echo "First: ", numbers[0]
echo "Last: ", numbers[^1]

# Add elements
numbers.add(6)
echo "\nAfter add: ", numbers

# Map
var squared = numbers.map(proc(x: int): int = x * x)
echo "\nMap (square): ", squared

# Filter
var evens = numbers.filter(proc(x: int): bool = x mod 2 == 0)
echo "Filter (even): ", evens

# Fold (reduce)
var sum = numbers.foldl(a + b)
echo "\nFold (sum): ", sum

# Sort
var unsorted = @[5, 2, 8, 1, 9, 3]
unsorted.sort()
echo "\nSorted: ", unsorted

# Reverse
numbers.reverse()
echo "Reversed: ", numbers

# Contains
echo "\nContains 3: ", numbers.contains(3)
echo "Contains 100: ", numbers.contains(100)

# Find
echo "Find (> 3): ", numbers.find(proc(x: int): bool = x > 3)

# Sequence comprehension
var squares = newSeq[int]()
for x in 1..5:
  squares.add(x * x)
echo "\nSquares: ", squares

# Concatenation
var combined = numbers & squares
echo "Combined: ", combined
