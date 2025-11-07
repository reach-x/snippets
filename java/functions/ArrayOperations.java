import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;

public class ArrayOperations {
    public static void main(String[] args) {
        ArrayList<Integer> numbers = new ArrayList<>(Arrays.asList(1, 2, 3, 4, 5));

        System.out.println("Numbers: " + numbers);
        System.out.println("Size: " + numbers.size());

        int sum = numbers.stream().mapToInt(Integer::intValue).sum();
        System.out.println("Sum: " + sum);

        int max = Collections.max(numbers);
        System.out.println("Max: " + max);

        int min = Collections.min(numbers);
        System.out.println("Min: " + min);

        numbers.add(6);
        System.out.println("After add: " + numbers);

        numbers.remove(numbers.size() - 1);
        System.out.println("After remove: " + numbers);

        ArrayList<Integer> squared = new ArrayList<>();
        for (int num : numbers) {
            squared.add(num * num);
        }
        System.out.println("Squared: " + squared);
    }
}
