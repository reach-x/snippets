#include <iostream>
#include <string>
#include <algorithm>
#include <cctype>

class StringUtils {
public:
    static std::string cleanPhone(const std::string& phoneNumber) {
        std::string result;
        std::copy_if(phoneNumber.begin(), phoneNumber.end(), std::back_inserter(result),
                     [](char c) { return std::isdigit(c); });
        return result;
    }

    static std::string toSnakeCase(const std::string& text) {
        std::string result;
        for (size_t i = 0; i < text.length(); i++) {
            if (std::isupper(text[i]) && i > 0) {
                result += '_';
            }
            result += std::tolower(text[i]);
        }
        return result;
    }

    static std::string toCamelCase(const std::string& text) {
        std::string result;
        bool capitalizeNext = false;

        for (char c : text) {
            if (c == '_') {
                capitalizeNext = true;
            } else if (capitalizeNext) {
                result += std::toupper(c);
                capitalizeNext = false;
            } else {
                result += c;
            }
        }
        return result;
    }

    static std::string truncate(const std::string& text, size_t length,
                                const std::string& suffix = "...") {
        if (text.length() <= length) {
            return text;
        }
        return text.substr(0, length - suffix.length()) + suffix;
    }

    static std::string slugify(const std::string& text) {
        std::string result = text;
        std::transform(result.begin(), result.end(), result.begin(), ::tolower);

        std::string slugged;
        for (char c : result) {
            if (std::isalnum(c) || c == ' ' || c == '-') {
                slugged += c;
            }
        }

        std::replace(slugged.begin(), slugged.end(), ' ', '-');
        return slugged;
    }

    static std::string reverse(const std::string& text) {
        std::string result = text;
        std::reverse(result.begin(), result.end());
        return result;
    }
};

int main() {
    std::cout << "String Utilities Test\n" << std::endl;

    std::string phone = "1-800-555-1234";
    std::cout << "Clean phone: " << StringUtils::cleanPhone(phone) << std::endl;

    std::string camel = "userName";
    std::cout << "To snake_case: " << StringUtils::toSnakeCase(camel) << std::endl;

    std::string snake = "user_name";
    std::cout << "To camelCase: " << StringUtils::toCamelCase(snake) << std::endl;

    std::string longText = "This is a very long text that needs to be truncated";
    std::cout << "Truncated: " << StringUtils::truncate(longText, 20) << std::endl;

    std::string title = "Hello World! This is a Test";
    std::cout << "Slugified: " << StringUtils::slugify(title) << std::endl;

    std::cout << "Reversed: " << StringUtils::reverse("hello") << std::endl;

    return 0;
}
