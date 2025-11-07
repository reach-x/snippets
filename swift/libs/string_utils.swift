#!/usr/bin/env swift

import Foundation

class StringUtils {
    static func cleanPhone(_ phoneNumber: String) -> String {
        return phoneNumber.components(separatedBy: CharacterSet.decimalDigits.inverted).joined()
    }

    static func toSnakeCase(_ text: String) -> String {
        let pattern = "([A-Z])"
        let regex = try! NSRegularExpression(pattern: pattern)
        let range = NSRange(text.startIndex..., in: text)
        let result = regex.stringByReplacingMatches(in: text, range: range, withTemplate: "_$1")
        return result.lowercased().trimmingCharacters(in: CharacterSet(charactersIn: "_"))
    }

    static func toCamelCase(_ text: String) -> String {
        let components = text.components(separatedBy: "_")
        let first = components.first ?? ""
        let rest = components.dropFirst().map { $0.capitalized }
        return ([first] + rest).joined()
    }

    static func truncate(_ text: String, length: Int, suffix: String = "...") -> String {
        if text.count <= length {
            return text
        }
        let endIndex = text.index(text.startIndex, offsetBy: length - suffix.count)
        return String(text[..<endIndex]) + suffix
    }

    static func slugify(_ text: String) -> String {
        return text
            .lowercased()
            .trimmingCharacters(in: .whitespaces)
            .replacingOccurrences(of: "[^\\w\\s-]", with: "", options: .regularExpression)
            .replacingOccurrences(of: "[-\\s]+", with: "-", options: .regularExpression)
    }
}

// Test the utilities
print("String Utilities Test\n")

let phone = "1-800-555-1234"
print("Clean phone: \(StringUtils.cleanPhone(phone))")

let camel = "userName"
print("To snake_case: \(StringUtils.toSnakeCase(camel))")

let snake = "user_name"
print("To camelCase: \(StringUtils.toCamelCase(snake))")

let longText = "This is a very long text that needs to be truncated"
print("Truncated: \(StringUtils.truncate(longText, length: 20))")

let title = "Hello World! This is a Test"
print("Slugified: \(StringUtils.slugify(title))")
