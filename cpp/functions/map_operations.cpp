#include <iostream>
#include <map>
#include <string>

int main() {
    std::map<std::string, std::string> person;
    person["name"] = "John";
    person["age"] = "30";
    person["city"] = "New York";

    std::cout << "Person map:" << std::endl;
    for (const auto& pair : person) {
        std::cout << "  " << pair.first << ": " << pair.second << std::endl;
    }

    std::cout << "\nName: " << person["name"] << std::endl;
    std::cout << "Age: " << person["age"] << std::endl;

    person["email"] = "john@example.com";
    std::cout << "\nAfter adding email:" << std::endl;
    for (const auto& pair : person) {
        std::cout << "  " << pair.first << ": " << pair.second << std::endl;
    }

    if (person.find("name") != person.end()) {
        std::cout << "\nName exists: " << person["name"] << std::endl;
    }

    person.erase("email");
    std::cout << "\nAfter erasing email:" << std::endl;
    for (const auto& pair : person) {
        std::cout << "  " << pair.first << ": " << pair.second << std::endl;
    }

    std::cout << "\nSize: " << person.size() << std::endl;

    return 0;
}
