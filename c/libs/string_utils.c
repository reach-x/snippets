#include <stdio.h>
#include <string.h>
#include <ctype.h>

void clean_phone(const char *input, char *output) {
    int j = 0;
    for (int i = 0; input[i]; i++) {
        if (isdigit(input[i])) {
            output[j++] = input[i];
        }
    }
    output[j] = '\0';
}

void to_upper_case(const char *input, char *output) {
    for (int i = 0; input[i]; i++) {
        output[i] = toupper(input[i]);
    }
    output[strlen(input)] = '\0';
}

void to_lower_case(const char *input, char *output) {
    for (int i = 0; input[i]; i++) {
        output[i] = tolower(input[i]);
    }
    output[strlen(input)] = '\0';
}

void str_reverse(const char *input, char *output) {
    int len = strlen(input);
    for (int i = 0; i < len; i++) {
        output[i] = input[len - 1 - i];
    }
    output[len] = '\0';
}

int main() {
    char buffer[100];

    printf("String Utilities Test\n\n");

    clean_phone("1-800-555-1234", buffer);
    printf("Clean phone: %s\n", buffer);

    to_upper_case("hello world", buffer);
    printf("Upper case: %s\n", buffer);

    to_lower_case("HELLO WORLD", buffer);
    printf("Lower case: %s\n", buffer);

    str_reverse("hello", buffer);
    printf("Reversed: %s\n", buffer);

    return 0;
}
