#!/usr/bin/env ruby

class Person
  attr_accessor :name, :age

  def initialize(name, age)
    @name = name
    @age = age
  end

  def greet
    "Hello, I'm #{@name} and I'm #{@age} years old"
  end

  def birthday
    @age += 1
    "Happy birthday! Now #{@age} years old"
  end

  def to_s
    "Person(name=#{@name}, age=#{@age})"
  end
end

class Employee < Person
  attr_accessor :job_title

  def initialize(name, age, job_title)
    super(name, age)
    @job_title = job_title
  end

  def work
    "#{@name} is working as a #{@job_title}"
  end

  def to_s
    "Employee(name=#{@name}, age=#{@age}, job=#{@job_title})"
  end
end

person = Person.new('Alice', 30)
puts person.greet
puts person.birthday
puts person

employee = Employee.new('Bob', 25, 'Software Engineer')
puts employee.greet
puts employee.work
puts employee
