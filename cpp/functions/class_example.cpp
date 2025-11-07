#include <iostream>
#include <string>

class Person {
protected:
    std::string name;
    int age;

public:
    Person(const std::string& name, int age) : name(name), age(age) {}

    std::string greet() const {
        return "Hello, I'm " + name + " and I'm " + std::to_string(age) + " years old";
    }

    void birthday() {
        age++;
        std::cout << "Happy birthday! Now " << age << " years old" << std::endl;
    }

    virtual void display() const {
        std::cout << "Person(name=" << name << ", age=" << age << ")" << std::endl;
    }

    virtual ~Person() = default;
};

class Employee : public Person {
private:
    std::string jobTitle;

public:
    Employee(const std::string& name, int age, const std::string& jobTitle)
        : Person(name, age), jobTitle(jobTitle) {}

    std::string work() const {
        return name + " is working as a " + jobTitle;
    }

    void display() const override {
        std::cout << "Employee(name=" << name << ", age=" << age
                  << ", job=" << jobTitle << ")" << std::endl;
    }
};

int main() {
    Person person("Alice", 30);
    std::cout << person.greet() << std::endl;
    person.birthday();
    person.display();

    std::cout << std::endl;

    Employee employee("Bob", 25, "Software Engineer");
    std::cout << employee.greet() << std::endl;
    std::cout << employee.work() << std::endl;
    employee.display();

    return 0;
}
