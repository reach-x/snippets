# Classes and methods in Crystal

puts "\n=== Classes and Methods in Crystal ===\n"

# Simple class
class Person
  property name : String
  property age : Int32

  def initialize(@name, @age)
  end

  def greet
    "Hello, I'm #{@name} and I'm #{@age} years old"
  end
end

person = Person.new("Alice", 30)
puts person.greet

# Inheritance
class Employee < Person
  property title : String

  def initialize(name, age, @title)
    super(name, age)
  end

  def info
    "#{greet}. I work as a #{@title}"
  end
end

employee = Employee.new("Bob", 25, "Developer")
puts "\n#{employee.info}"

# Module
module Calculable
  def square(x)
    x * x
  end
end

class Calculator
  include Calculable

  def cube(x)
    x * x * x
  end
end

calc = Calculator.new
puts "\nSquare 5: #{calc.square(5)}"
puts "Cube 3: #{calc.cube(3)}"

# Generic class
class Box(T)
  property value : T

  def initialize(@value)
  end

  def get
    @value
  end
end

int_box = Box.new(42)
string_box = Box.new("hello")

puts "\nInt box: #{int_box.get}"
puts "String box: #{string_box.get}"
