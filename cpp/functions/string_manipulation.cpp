#include <iostream>
#include <string>
#include <algorithm>

int main() {
    std::string text = "  Hello, World!  ";

    std::cout << "Original: '" << text << "'" << std::endl;

    std::string upper = text;
    std::transform(upper.begin(), upper.end(), upper.begin(), ::toupper);
    std::cout << "Upper: '" << upper << "'" << std::endl;

    std::string lower = text;
    std::transform(lower.begin(), lower.end(), lower.begin(), ::tolower);
    std::cout << "Lower: '" << lower << "'" << std::endl;

    // Trim
    std::string trimmed = text;
    trimmed.erase(0, trimmed.find_first_not_of(" \t\n\r"));
    trimmed.erase(trimmed.find_last_not_of(" \t\n\r") + 1);
    std::cout << "Trim: '" << trimmed << "'" << std::endl;

    std::string replaced = text;
    size_t pos = replaced.find("World");
    if (pos != std::string::npos) {
        replaced.replace(pos, 5, "C++");
    }
    std::cout << "Replace: '" << replaced << "'" << std::endl;

    std::cout << "Length: " << text.length() << std::endl;
    std::cout << "Contains 'World': " << (text.find("World") != std::string::npos) << std::endl;
    std::cout << "Substring: '" << trimmed.substr(0, 5) << "'" << std::endl;

    return 0;
}
