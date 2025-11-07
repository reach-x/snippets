#!/usr/bin/env swift

let text = "  Hello, World!  "

print("Original: '\(text)'")
print("Upper: '\(text.uppercased())'")
print("Lower: '\(text.lowercased())'")
print("Trim: '\(text.trimmingCharacters(in: .whitespaces))'")
print("Replace: '\(text.replacingOccurrences(of: "World", with: "Swift"))'")
print("Split: \(text.trimmingCharacters(in: .whitespaces).split(separator: ","))")
print("Contains: \(text.contains("World"))")
print("HasPrefix: \(text.trimmingCharacters(in: .whitespaces).hasPrefix("Hello"))")
print("HasSuffix: \(text.trimmingCharacters(in: .whitespaces).hasSuffix("!"))")
print("Length: \(text.count)")
print("Substring: '\(text.trimmingCharacters(in: .whitespaces).prefix(5))'")
