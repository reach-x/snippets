fun main() {
    var name: String? = "John"
    var age: Int? = null

    // Safe call
    println("Name length: ${name?.length}")
    println("Age plus one: ${age?.plus(1)}")

    // Elvis operator
    val displayAge = age ?: 0
    println("Age with default: $displayAge")

    // Let function
    name?.let {
        println("Name is not null: $it")
    }

    age?.let {
        println("Age is not null: $it")
    } ?: println("Age is null")

    // Safe cast
    val value: Any = "Hello"
    val str = value as? String
    println("Safe cast: $str")

    val number = value as? Int
    println("Safe cast to Int: $number")

    // Not-null assertion (use carefully!)
    val nonNullName = name!!
    println("Non-null name: $nonNullName")
}
