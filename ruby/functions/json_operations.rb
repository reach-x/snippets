#!/usr/bin/env ruby

require 'json'

data = {
  name: 'John Doe',
  age: 30,
  skills: ['Ruby', 'Rails', 'JavaScript'],
  active: true
}

json_string = JSON.pretty_generate(data)
puts "JSON String:\n#{json_string}"

parsed_data = JSON.parse(json_string)
puts "\nParsed data: #{parsed_data}"
puts "Name: #{parsed_data['name']}"
puts "Skills: #{parsed_data['skills'].join(', ')}"

json_file = '../tmp/data.json'
File.write(json_file, json_string)
puts "\nWritten to #{json_file}"

loaded_data = JSON.parse(File.read(json_file))
puts "Loaded from file: #{loaded_data}"
