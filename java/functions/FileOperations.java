import java.io.*;
import java.nio.file.*;

public class FileOperations {
    public static void main(String[] args) {
        String testFile = "../tmp/test_output.txt";

        try {
            String data = "Hello from Java!\nThis is a test file.\n";
            Files.write(Paths.get(testFile), data.getBytes());
            System.out.println("Written to " + testFile);

            String content = new String(Files.readAllBytes(Paths.get(testFile)));
            System.out.println("Read content:\n" + content);

            if (Files.exists(Paths.get(testFile))) {
                System.out.println("File exists: " + testFile);
                System.out.println("File size: " + Files.size(Paths.get(testFile)) + " bytes");
            }

            Files.write(Paths.get(testFile), "Appended line\n".getBytes(), StandardOpenOption.APPEND);
            System.out.println("Appended to file");

        } catch (IOException e) {
            System.err.println("Error: " + e.getMessage());
        }
    }
}
