object StringManipulation {
  def main(args: Array[String]): Unit = {
    val text = "  Hello, World!  "

    println(s"Original: '$text'")
    println(s"Upper: '${text.toUpperCase}'")
    println(s"Lower: '${text.toLowerCase}'")
    println(s"Trim: '${text.trim}'")
    println(s"Replace: '${text.replace("World", "Scala")}'")
    println(s"Split: ${text.trim.split(", ").mkString("[", ", ", "]")}")
    println(s"Contains: ${text.contains("World")}")
    println(s"StartsWith: ${text.trim.startsWith("Hello")}")
    println(s"EndsWith: ${text.trim.endsWith("!")}")
    println(s"Length: ${text.length}")
    println(s"Substring: '${text.trim.substring(0, 5)}'")
    println(s"Repeat: '${"-" * 20}'")
  }
}
