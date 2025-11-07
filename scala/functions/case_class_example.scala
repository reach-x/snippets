// Case class - immutable data structure
case class Person(name: String, age: Int) {
  def greet(): String = s"Hello, I'm $name and I'm $age years old"

  def birthday(): Person = copy(age = age + 1)
}

case class Employee(person: Person, jobTitle: String) {
  def work(): String = s"${person.name} is working as a $jobTitle"
}

// Pattern matching example
sealed trait Animal
case class Dog(name: String) extends Animal
case class Cat(name: String) extends Animal
case class Bird(name: String, canFly: Boolean) extends Animal

object CaseClassExample {
  def main(args: Array[String]): Unit = {
    val person = Person("Alice", 30)
    println(person.greet())

    val olderPerson = person.birthday()
    println(s"After birthday: ${olderPerson.greet()}")

    val employee = Employee(Person("Bob", 25), "Software Engineer")
    println(employee.work())

    // Pattern matching
    val animals = List(
      Dog("Rex"),
      Cat("Whiskers"),
      Bird("Tweety", true)
    )

    animals.foreach {
      case Dog(name) => println(s"$name is a dog")
      case Cat(name) => println(s"$name is a cat")
      case Bird(name, true) => println(s"$name is a bird that can fly")
      case Bird(name, false) => println(s"$name is a bird that cannot fly")
    }

    // Option type
    val maybeValue: Option[Int] = Some(42)
    val noneValue: Option[Int] = None

    println(s"Maybe value: ${maybeValue.getOrElse(0)}")
    println(s"None value: ${noneValue.getOrElse(0)}")
  }
}
