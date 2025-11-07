#include <stdio.h>
#include <string.h>

typedef struct {
    char name[50];
    int age;
} Person;

typedef struct {
    Person person;
    char job_title[50];
} Employee;

void person_greet(Person *p) {
    printf("Hello, I'm %s and I'm %d years old\n", p->name, p->age);
}

void person_birthday(Person *p) {
    p->age++;
    printf("Happy birthday! Now %d years old\n", p->age);
}

void employee_work(Employee *e) {
    printf("%s is working as a %s\n", e->person.name, e->job_title);
}

int main() {
    Person person;
    strcpy(person.name, "Alice");
    person.age = 30;

    person_greet(&person);
    person_birthday(&person);
    printf("Person { name: %s, age: %d }\n", person.name, person.age);

    Employee employee;
    strcpy(employee.person.name, "Bob");
    employee.person.age = 25;
    strcpy(employee.job_title, "Software Engineer");

    person_greet(&employee.person);
    employee_work(&employee);
    printf("Employee { name: %s, age: %d, job: %s }\n",
           employee.person.name, employee.person.age, employee.job_title);

    return 0;
}
