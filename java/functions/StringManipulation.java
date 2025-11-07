public class StringManipulation {
    public static void main(String[] args) {
        String text = "  Hello, World!  ";

        System.out.println("Original: '" + text + "'");
        System.out.println("Upper: '" + text.toUpperCase() + "'");
        System.out.println("Lower: '" + text.toLowerCase() + "'");
        System.out.println("Trim: '" + text.trim() + "'");
        System.out.println("Replace: '" + text.replace("World", "Java") + "'");
        System.out.println("StartsWith: " + text.trim().startsWith("Hello"));
        System.out.println("EndsWith: " + text.trim().endsWith("!"));
        System.out.println("Length: " + text.trim().length());
        System.out.println("Substring: '" + text.trim().substring(0, 5) + "'");
    }
}
