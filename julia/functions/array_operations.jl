#!/usr/bin/env julia
# Array operations in Julia

println("\n=== Array Operations in Julia ===\n")

# Create arrays
numbers = [1, 2, 3, 4, 5]
matrix = [1 2 3; 4 5 6; 7 8 9]

println("Numbers: ", numbers)
println("Matrix: ", matrix)

# Array properties
println("\nLength: ", length(numbers))
println("Size: ", size(matrix))
println("Dimensions: ", ndims(matrix))

# Element access
println("\nFirst element: ", numbers[1])  # 1-indexed
println("Last element: ", numbers[end])

# Array operations
println("\nSquare (broadcasting): ", numbers .^ 2)
println("Sum: ", sum(numbers))
println("Product: ", prod(numbers))
println("Mean: ", mean(numbers))
println("Maximum: ", maximum(numbers))
println("Minimum: ", minimum(numbers))

# Map and filter
println("\nMap (square): ", map(x -> x^2, numbers))
println("Filter (even): ", filter(x -> x % 2 == 0, numbers))

# Comprehensions
squares = [x^2 for x in 1:5]
println("\nList comprehension: ", squares)

even_squares = [x^2 for x in 1:10 if x % 2 == 0]
println("Conditional comprehension: ", even_squares)

# Range
println("\nRange 1:10: ", collect(1:10))
println("Range 1:2:10: ", collect(1:2:10))

# Push and append
push!(numbers, 6)
println("\nAfter push!: ", numbers)

append!(numbers, [7, 8, 9])
println("After append!: ", numbers)

# Sort
unsorted = [5, 2, 8, 1, 9, 3]
println("\nSorted: ", sort(unsorted))
println("Sorted (descending): ", sort(unsorted, rev=true))

# Reverse
println("Reversed: ", reverse(numbers))

# Matrix operations
println("\nMatrix transpose: ", transpose(matrix))
println("Matrix sum: ", sum(matrix))

# Broadcasting
println("\nBroadcasting (+1): ", numbers .+ 1)
println("Broadcasting (*2): ", numbers .* 2)
