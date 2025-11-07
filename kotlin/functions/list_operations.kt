fun main() {
    val numbers = mutableListOf(1, 2, 3, 4, 5)

    println("Numbers: $numbers")
    println("Size: ${numbers.size}")
    println("Sum: ${numbers.sum()}")
    println("Max: ${numbers.maxOrNull()}")
    println("Min: ${numbers.minOrNull()}")

    numbers.add(6)
    println("After add: $numbers")

    numbers.removeLast()
    println("After removeLast: $numbers")

    val squared = numbers.map { it * it }
    println("Squared: $squared")

    val evens = numbers.filter { it % 2 == 0 }
    println("Even numbers: $evens")

    val sorted = numbers.sortedDescending()
    println("Sorted descending: $sorted")

    println("First element: ${numbers.first()}")
    println("Last element: ${numbers.last()}")

    val unique = listOf(1, 2, 2, 3, 3, 3).distinct()
    println("Unique: $unique")
}
