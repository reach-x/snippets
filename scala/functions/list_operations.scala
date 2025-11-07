object ListOperations {
  def main(args: Array[String]): Unit = {
    val numbers = List(1, 2, 3, 4, 5)

    println(s"Numbers: $numbers")
    println(s"Length: ${numbers.length}")
    println(s"Sum: ${numbers.sum}")
    println(s"Max: ${numbers.max}")
    println(s"Min: ${numbers.min}")

    val withSix = numbers :+ 6
    println(s"After append: $withSix")

    val squared = numbers.map(x => x * x)
    println(s"Squared: $squared")

    val evens = numbers.filter(_ % 2 == 0)
    println(s"Even numbers: $evens")

    val doubled = numbers.map(_ * 2)
    println(s"Doubled: $doubled")

    // Pattern matching
    numbers.foreach {
      case x if x % 2 == 0 => println(s"$x is even")
      case x => println(s"$x is odd")
    }

    // Fold/reduce
    val product = numbers.fold(1)(_ * _)
    println(s"Product: $product")
  }
}
