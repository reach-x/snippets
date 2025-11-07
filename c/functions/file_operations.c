#include <stdio.h>
#include <stdlib.h>

int main() {
    const char *test_file = "../tmp/test_output.txt";
    FILE *file;

    // Write to file
    file = fopen(test_file, "w");
    if (file == NULL) {
        printf("Error opening file for writing\n");
        return 1;
    }

    fprintf(file, "Hello from C!\n");
    fprintf(file, "This is a test file.\n");
    fclose(file);
    printf("Written to %s\n", test_file);

    // Read from file
    file = fopen(test_file, "r");
    if (file == NULL) {
        printf("Error opening file for reading\n");
        return 1;
    }

    printf("Read content:\n");
    char buffer[256];
    while (fgets(buffer, sizeof(buffer), file) != NULL) {
        printf("%s", buffer);
    }
    fclose(file);

    // Check file size
    file = fopen(test_file, "r");
    if (file != NULL) {
        fseek(file, 0, SEEK_END);
        long file_size = ftell(file);
        printf("\nFile size: %ld bytes\n", file_size);
        fclose(file);
    }

    // Append to file
    file = fopen(test_file, "a");
    if (file != NULL) {
        fprintf(file, "Appended line\n");
        fclose(file);
        printf("Appended to file\n");
    }

    return 0;
}
