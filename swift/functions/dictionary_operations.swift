#!/usr/bin/env swift

var person: [String: Any] = [
    "name": "John",
    "age": 30,
    "city": "New York"
]

print("Person: \(person)")
print("Name: \(person["name"] ?? "")")
print("Age: \(person["age"] ?? 0)")
print("Keys: \(person.keys)")
print("Values: \(person.values)")

person["email"] = "john@example.com"
print("\nAfter adding email: \(person)")

if let name = person["name"] {
    print("\nName exists: \(name)")
}

print("\nIterating over dictionary:")
for (key, value) in person {
    print("  \(key): \(value)")
}

person.removeValue(forKey: "email")
print("\nAfter removing email: \(person)")

print("Count: \(person.count)")
