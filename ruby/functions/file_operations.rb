#!/usr/bin/env ruby

test_file = '../tmp/test_output.txt'

data = "Hello from Ruby!\nThis is a test file.\n"
File.write(test_file, data)
puts "Written to #{test_file}"

content = File.read(test_file)
puts "Read content:\n#{content}"

lines = File.readlines(test_file)
puts "Lines: #{lines}"

if File.exist?(test_file)
  puts "File exists: #{test_file}"
  puts "File size: #{File.size(test_file)} bytes"
end

File.open(test_file, 'a') do |file|
  file.puts 'Appended line'
end
puts "Appended to file"

Dir.foreach('../tmp') do |filename|
  next if filename == '.' || filename == '..'
  puts "Found file: #{filename}"
end
