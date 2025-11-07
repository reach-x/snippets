#!/usr/bin/env swift

import Foundation

struct Person: Codable {
    let name: String
    let age: Int
    let skills: [String]
    let active: Bool
}

let person = Person(
    name: "John Doe",
    age: 30,
    skills: ["Swift", "iOS", "SwiftUI"],
    active: true
)

// Encode to JSON
let encoder = JSONEncoder()
encoder.outputFormatting = .prettyPrinted

if let jsonData = try? encoder.encode(person) {
    if let jsonString = String(data: jsonData, encoding: .utf8) {
        print("JSON String:")
        print(jsonString)
    }

    // Decode from JSON
    let decoder = JSONDecoder()
    if let decodedPerson = try? decoder.decode(Person.self, from: jsonData) {
        print("\nDecoded person:")
        print("Name: \(decodedPerson.name)")
        print("Age: \(decodedPerson.age)")
        print("Skills: \(decodedPerson.skills.joined(separator: ", "))")
    }
}

// Working with JSON dictionaries
let jsonDict: [String: Any] = [
    "title": "Hello",
    "count": 42,
    "active": true
]

if let jsonData = try? JSONSerialization.data(withJSONObject: jsonDict, options: .prettyPrinted) {
    if let jsonString = String(data: jsonData, encoding: .utf8) {
        print("\nDictionary as JSON:")
        print(jsonString)
    }
}
