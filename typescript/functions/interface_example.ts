interface Person {
    name: string;
    age: number;
    email?: string;
}

interface Employee extends Person {
    jobTitle: string;
    department: string;
}

const person: Person = {
    name: 'John Doe',
    age: 30
};

console.log(`Person: ${JSON.stringify(person, null, 2)}`);

person.email = 'john@example.com';
console.log(`With email: ${JSON.stringify(person, null, 2)}`);

const employee: Employee = {
    name: 'Alice Smith',
    age: 28,
    jobTitle: 'Software Engineer',
    department: 'Engineering'
};

console.log(`Employee: ${JSON.stringify(employee, null, 2)}`);

function greet(person: Person): string {
    return `Hello, ${person.name}!`;
}

console.log(greet(person));
console.log(greet(employee));
