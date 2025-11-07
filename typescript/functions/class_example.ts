class Person {
    protected name: string;
    protected age: number;

    constructor(name: string, age: number) {
        this.name = name;
        this.age = age;
    }

    greet(): string {
        return `Hello, I'm ${this.name} and I'm ${this.age} years old`;
    }

    birthday(): string {
        this.age++;
        return `Happy birthday! Now ${this.age} years old`;
    }

    toString(): string {
        return `Person(name=${this.name}, age=${this.age})`;
    }
}

class Employee extends Person {
    private jobTitle: string;

    constructor(name: string, age: number, jobTitle: string) {
        super(name, age);
        this.jobTitle = jobTitle;
    }

    work(): string {
        return `${this.name} is working as a ${this.jobTitle}`;
    }

    override toString(): string {
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
