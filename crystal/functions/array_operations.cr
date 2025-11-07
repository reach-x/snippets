# Array operations in Crystal

puts "\n=== Array Operations in Crystal ===\n"

# Create arrays
numbers = [1, 2, 3, 4, 5]
fruits = ["apple", "banana", "cherry"]

puts "Numbers: #{numbers}"
puts "Fruits: #{fruits}"

# Array length
puts "\nLength: #{numbers.size}"

# Access elements
puts "First: #{numbers.first}"
puts "Last: #{numbers.last}"
puts "Element at 2: #{numbers[2]}"

# Add elements
numbers << 6
puts "\nAfter <<  6: #{numbers}"

numbers += [7, 8, 9]
puts "After += [7,8,9]: #{numbers}"

# Contains
puts "\nIncludes 3: #{numbers.includes?(3)}"
puts "Includes 100: #{numbers.includes?(100)}"

# Map
squared = numbers.map { |x| x * x }
puts "\nMap (square): #{squared}"

# Select (filter)
evens = numbers.select { |x| x % 2 == 0 }
puts "Select (even): #{evens}"

# Reduce
sum = numbers.reduce { |a, b| a + b }
puts "\nReduce (sum): #{sum}"

# Any and all
puts "\nAny > 5: #{numbers.any? { |x| x > 5 }}"
puts "All > 0: #{numbers.all? { |x| x > 0 }}"

# Sort
unsorted = [5, 2, 8, 1, 9, 3]
puts "\nSorted: #{unsorted.sort}"
puts "Sorted (reverse): #{unsorted.sort.reverse}"

# Uniq
with_dupes = [1, 2, 2, 3, 3, 3, 4]
puts "\nUniq: #{with_dupes.uniq}"

# First and last n elements
puts "\nFirst 3: #{numbers.first(3)}"
puts "Last 2: #{numbers.last(2)}"

# Array comprehension
squares = (1..5).map { |x| x * x }.to_a
puts "\nSquares: #{squares}"
