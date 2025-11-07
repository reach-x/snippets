fun main() {
    val person = mutableMapOf(
        "name" to "John",
        "age" to "30",
        "city" to "New York"
    )

    println("Person: $person")
    println("Name: ${person["name"]}")
    println("Age: ${person["age"]}")
    println("Keys: ${person.keys}")
    println("Values: ${person.values}")

    person["email"] = "john@example.com"
    println("\nAfter adding email: $person")

    if ("name" in person) {
        println("\nName exists: ${person["name"]}")
    }

    println("\nIterating over map:")
    for ((key, value) in person) {
        println("  $key: $value")
    }

    person.remove("email")
    println("\nAfter removing email: $person")

    println("Size: ${person.size}")
}
