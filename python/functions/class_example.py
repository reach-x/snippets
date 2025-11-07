#!/usr/bin/env python3

class Person:
    def __init__(self, name, age):
        self.name = name
        self.age = age

    def greet(self):
        return f"Hello, I'm {self.name} and I'm {self.age} years old"

    def birthday(self):
        self.age += 1
        return f"Happy birthday! Now {self.age} years old"

    def __str__(self):
        return f"Person(name={self.name}, age={self.age})"


class Employee(Person):
    def __init__(self, name, age, job_title):
        super().__init__(name, age)
        self.job_title = job_title

    def work(self):
        return f"{self.name} is working as a {self.job_title}"

    def __str__(self):
        return f"Employee(name={self.name}, age={self.age}, job={self.job_title})"


person = Person("Alice", 30)
print(person.greet())
print(person.birthday())
print(person)

employee = Employee("Bob", 25, "Software Engineer")
print(employee.greet())
print(employee.work())
print(employee)
