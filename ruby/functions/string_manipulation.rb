#!/usr/bin/env ruby

text = "  Hello, World!  "

puts "Original: '#{text}'"
puts "Upper: '#{text.upcase}'"
puts "Lower: '#{text.downcase}'"
puts "Strip: '#{text.strip}'"
puts "Replace: '#{text.gsub('World', 'Ruby')}'"
puts "Split: #{text.strip.split(', ')}"
puts "Include?: #{text.include?('World')}"
puts "Start with?: #{text.strip.start_with?('Hello')}"
puts "End with?: #{text.strip.end_with?('!')}"
puts "Length: #{text.strip.length}"
puts "Repeat: '##{'-' * 20}'"
puts "Reverse: '#{text.strip.reverse}'"
