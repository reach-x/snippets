open class Person(var name: String, var age: Int) {
    fun greet(): String {
        return "Hello, I'm $name and I'm $age years old"
    }

    fun birthday() {
        age++
        println("Happy birthday! Now $age years old")
    }

    override fun toString(): String {
        return "Person(name=$name, age=$age)"
    }
}

class Employee(name: String, age: Int, val jobTitle: String) : Person(name, age) {
    fun work(): String {
        return "$name is working as a $jobTitle"
    }

    override fun toString(): String {
        return "Employee(name=$name, age=$age, job=$jobTitle)"
    }
}

// Data class example
data class Product(val name: String, val price: Double, val quantity: Int)

fun main() {
    val person = Person("Alice", 30)
    println(person.greet())
    person.birthday()
    println(person)

    println()

    val employee = Employee("Bob", 25, "Software Engineer")
    println(employee.greet())
    println(employee.work())
    println(employee)

    println()

    val product = Product("Laptop", 999.99, 5)
    println("Product: $product")

    // Data class features
    val product2 = product.copy(quantity = 10)
    println("Copied product: $product2")
}
