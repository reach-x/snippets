public class StringUtils {
    public static String cleanPhone(String phoneNumber) {
        return phoneNumber.replaceAll("[^0-9]", "");
    }

    public static String toSnakeCase(String text) {
        return text.replaceAll("([A-Z])", "_$1").toLowerCase().replaceFirst("^_", "");
    }

    public static String toCamelCase(String text) {
        StringBuilder result = new StringBuilder();
        boolean capitalizeNext = false;

        for (char c : text.toCharArray()) {
            if (c == '_') {
                capitalizeNext = true;
            } else if (capitalizeNext) {
                result.append(Character.toUpperCase(c));
                capitalizeNext = false;
            } else {
                result.append(c);
            }
        }
        return result.toString();
    }

    public static String truncate(String text, int length, String suffix) {
        if (text.length() <= length) {
            return text;
        }
        return text.substring(0, length - suffix.length()) + suffix;
    }

    public static String slugify(String text) {
        return text.toLowerCase()
            .trim()
            .replaceAll("[^\\w\\s-]", "")
            .replaceAll("[-\\s]+", "-");
    }

    public static void main(String[] args) {
        System.out.println("String Utilities Test\n");

        String phone = "1-800-555-1234";
        System.out.println("Clean phone: " + cleanPhone(phone));

        String camel = "userName";
        System.out.println("To snake_case: " + toSnakeCase(camel));

        String snake = "user_name";
        System.out.println("To camelCase: " + toCamelCase(snake));

        String longText = "This is a very long text that needs to be truncated";
        System.out.println("Truncated: " + truncate(longText, 20, "..."));

        String title = "Hello World! This is a Test";
        System.out.println("Slugified: " + slugify(title));
    }
}
