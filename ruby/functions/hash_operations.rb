#!/usr/bin/env ruby

person = {
  name: 'John',
  age: 30,
  city: 'New York'
}

puts "Person: #{person}"
puts "Name: #{person[:name]}"
puts "Age: #{person[:age]}"
puts "Keys: #{person.keys}"
puts "Values: #{person.values}"

person[:email] = 'john@example.com'
puts "After adding email: #{person}"

if person.key?(:name)
  puts "Name exists: #{person[:name]}"
end

puts "\nIterating over hash:"
person.each do |key, value|
  puts "  #{key}: #{value}"
end

person.delete(:email)
puts "After deleting email: #{person}"

merged = person.merge(country: 'USA', state: 'NY')
puts "Merged: #{merged}"
