#!/usr/bin/env node

class Person {
    constructor(name, age) {
        this.name = name;
        this.age = age;
    }

    greet() {
        return `Hello, I'm ${this.name} and I'm ${this.age} years old`;
    }

    birthday() {
        this.age++;
        return `Happy birthday! Now ${this.age} years old`;
    }

    toString() {
        return `Person(name=${this.name}, age=${this.age})`;
    }
}

class Employee extends Person {
    constructor(name, age, jobTitle) {
        super(name, age);
        this.jobTitle = jobTitle;
    }

    work() {
        return `${this.name} is working as a ${this.jobTitle}`;
    }

    toString() {
        return `Employee(name=${this.name}, age=${this.age}, job=${this.jobTitle})`;
    }
}

const person = new Person('Alice', 30);
console.log(person.greet());
console.log(person.birthday());
console.log(person.toString());

const employee = new Employee('Bob', 25, 'Software Engineer');
console.log(employee.greet());
console.log(employee.work());
console.log(employee.toString());
