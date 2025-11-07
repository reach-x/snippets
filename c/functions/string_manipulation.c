#include <stdio.h>
#include <string.h>
#include <ctype.h>

int main() {
    char text[] = "  Hello, World!  ";
    char buffer[100];

    printf("Original: '%s'\n", text);

    // Convert to uppercase
    strcpy(buffer, text);
    for (int i = 0; buffer[i]; i++) {
        buffer[i] = toupper(buffer[i]);
    }
    printf("Upper: '%s'\n", buffer);

    // Convert to lowercase
    strcpy(buffer, text);
    for (int i = 0; buffer[i]; i++) {
        buffer[i] = tolower(buffer[i]);
    }
    printf("Lower: '%s'\n", buffer);

    // Trim (simple version - removes leading/trailing spaces)
    char *start = text;
    while (*start == ' ') start++;

    char *end = text + strlen(text) - 1;
    while (end > text && *end == ' ') end--;

    int len = end - start + 1;
    strncpy(buffer, start, len);
    buffer[len] = '\0';
    printf("Trim: '%s'\n", buffer);

    printf("Length: %lu\n", strlen(text));
    printf("Contains 'World': %s\n", strstr(text, "World") ? "true" : "false");

    return 0;
}
