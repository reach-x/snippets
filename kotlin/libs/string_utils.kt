object StringUtils {
    fun cleanPhone(phoneNumber: String): String {
        return phoneNumber.filter { it.isDigit() }
    }

    fun toSnakeCase(text: String): String {
        return text.replace(Regex("([A-Z])"), "_$1")
            .lowercase()
            .trimStart('_')
    }

    fun toCamelCase(text: String): String {
        return text.split("_")
            .mapIndexed { index, s ->
                if (index == 0) s else s.capitalize()
            }
            .joinToString("")
    }

    fun truncate(text: String, length: Int, suffix: String = "..."): String {
        return if (text.length <= length) {
            text
        } else {
            text.substring(0, length - suffix.length) + suffix
        }
    }

    fun slugify(text: String): String {
        return text.lowercase()
            .trim()
            .replace(Regex("[^\\w\\s-]"), "")
            .replace(Regex("[-\\s]+"), "-")
    }

    fun isPalindrome(text: String): Boolean {
        val cleaned = text.lowercase().filter { it.isLetterOrDigit() }
        return cleaned == cleaned.reversed()
    }
}

fun main() {
    println("String Utilities Test\n")

    val phone = "1-800-555-1234"
    println("Clean phone: ${StringUtils.cleanPhone(phone)}")

    val camel = "userName"
    println("To snake_case: ${StringUtils.toSnakeCase(camel)}")

    val snake = "user_name"
    println("To camelCase: ${StringUtils.toCamelCase(snake)}")

    val longText = "This is a very long text that needs to be truncated"
    println("Truncated: ${StringUtils.truncate(longText, 20)}")

    val title = "Hello World! This is a Test"
    println("Slugified: ${StringUtils.slugify(title)}")

    println("Is palindrome 'racecar': ${StringUtils.isPalindrome("racecar")}")
    println("Is palindrome 'hello': ${StringUtils.isPalindrome("hello")}")
}
