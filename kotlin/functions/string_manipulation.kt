fun main() {
    val text = "  Hello, World!  "

    println("Original: '$text'")
    println("Upper: '${text.uppercase()}'")
    println("Lower: '${text.lowercase()}'")
    println("Trim: '${text.trim()}'")
    println("Replace: '${text.replace("World", "Kotlin")}'")
    println("Split: ${text.trim().split(", ")}")
    println("Contains: ${text.contains("World")}")
    println("StartsWith: ${text.trim().startsWith("Hello")}")
    println("EndsWith: ${text.trim().endsWith("!")}")
    println("Length: ${text.length}")
    println("Substring: '${text.trim().substring(0, 5)}'")
    println("Repeat: '${"-".repeat(20)}'")
    println("Reversed: '${text.trim().reversed()}'")
}
