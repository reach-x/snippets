#include <stdio.h>

int main() {
    int numbers[] = {1, 2, 3, 4, 5};
    int length = sizeof(numbers) / sizeof(numbers[0]);

    printf("Numbers: ");
    for (int i = 0; i < length; i++) {
        printf("%d ", numbers[i]);
    }
    printf("\n");

    printf("Length: %d\n", length);

    // Calculate sum
    int sum = 0;
    for (int i = 0; i < length; i++) {
        sum += numbers[i];
    }
    printf("Sum: %d\n", sum);

    // Find max
    int max = numbers[0];
    for (int i = 1; i < length; i++) {
        if (numbers[i] > max) {
            max = numbers[i];
        }
    }
    printf("Max: %d\n", max);

    // Find min
    int min = numbers[0];
    for (int i = 1; i < length; i++) {
        if (numbers[i] < min) {
            min = numbers[i];
        }
    }
    printf("Min: %d\n", min);

    // Calculate average
    float average = (float)sum / length;
    printf("Average: %.2f\n", average);

    return 0;
}
