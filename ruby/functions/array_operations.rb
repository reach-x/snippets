#!/usr/bin/env ruby

numbers = [1, 2, 3, 4, 5]

puts "Numbers: #{numbers}"
puts "Length: #{numbers.length}"
puts "Sum: #{numbers.sum}"
puts "Max: #{numbers.max}"
puts "Min: #{numbers.min}"

numbers.push(6)
puts "After push: #{numbers}"

numbers.pop
puts "After pop: #{numbers}"

squared = numbers.map { |x| x ** 2 }
puts "Squared: #{squared}"

evens = numbers.select { |x| x.even? }
puts "Even numbers: #{evens}"

puts "First element: #{numbers.first}"
puts "Last element: #{numbers.last}"

puts "Sorted descending: #{numbers.sort.reverse}"
puts "Unique: #{[1, 2, 2, 3, 3, 3].uniq}"
