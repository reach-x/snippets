#!/usr/bin/env swift

class Person {
    var name: String
    var age: Int

    init(name: String, age: Int) {
        self.name = name
        self.age = age
    }

    func greet() -> String {
        return "Hello, I'm \(name) and I'm \(age) years old"
    }

    func birthday() {
        age += 1
        print("Happy birthday! Now \(age) years old")
    }

    func description() -> String {
        return "Person(name=\(name), age=\(age))"
    }
}

class Employee: Person {
    var jobTitle: String

    init(name: String, age: Int, jobTitle: String) {
        self.jobTitle = jobTitle
        super.init(name: name, age: age)
    }

    func work() -> String {
        return "\(name) is working as a \(jobTitle)"
    }

    override func description() -> String {
        return "Employee(name=\(name), age=\(age), job=\(jobTitle))"
    }
}

let person = Person(name: "Alice", age: 30)
print(person.greet())
person.birthday()
print(person.description())

print("")

let employee = Employee(name: "Bob", age: 25, jobTitle: "Software Engineer")
print(employee.greet())
print(employee.work())
print(employee.description())
