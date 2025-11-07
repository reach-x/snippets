#!/usr/bin/env swift

import Foundation

var name: String? = "John"
var age: Int? = nil

// Optional binding
if let unwrappedName = name {
    print("Name is: \(unwrappedName)")
} else {
    print("Name is nil")
}

// Nil coalescing
let displayAge = age ?? 0
print("Age (with default): \(displayAge)")

// Optional chaining
let uppercaseName = name?.uppercased()
print("Uppercase name: \(uppercaseName ?? "nil")")

// Guard statement
func greet(person: String?) {
    guard let name = person else {
        print("No name provided")
        return
    }
    print("Hello, \(name)!")
}

greet(person: name)
greet(person: nil)

// Implicitly unwrapped optionals
var email: String! = "john@example.com"
print("Email: \(email!)")
