#include <iostream>
#include <string>

template <typename T>
T maximum(T a, T b) {
    return (a > b) ? a : b;
}

template <typename T>
class Box {
private:
    T value;

public:
    Box(T value) : value(value) {}

    T getValue() const {
        return value;
    }

    void setValue(T value) {
        this->value = value;
    }

    void display() const {
        std::cout << "Box contains: " << value << std::endl;
    }
};

template <typename K, typename V>
class KeyValuePair {
private:
    K key;
    V value;

public:
    KeyValuePair(K key, V value) : key(key), value(value) {}

    void display() const {
        std::cout << "Key: " << key << ", Value: " << value << std::endl;
    }
};

int main() {
    std::cout << "Maximum of 10 and 20: " << maximum(10, 20) << std::endl;
    std::cout << "Maximum of 3.5 and 2.1: " << maximum(3.5, 2.1) << std::endl;
    std::cout << "Maximum of 'a' and 'z': " << maximum('a', 'z') << std::endl;

    Box<int> intBox(123);
    intBox.display();

    Box<std::string> stringBox("Hello C++");
    stringBox.display();

    KeyValuePair<std::string, int> pair("age", 30);
    pair.display();

    return 0;
}
