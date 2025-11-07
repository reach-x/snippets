#!/usr/bin/env node

const person = {
    name: 'John',
    age: 30,
    city: 'New York'
};

console.log(`Person: ${JSON.stringify(person, null, 2)}`);
console.log(`Name: ${person.name}`);
console.log(`Age: ${person['age']}`);
console.log(`Keys: ${Object.keys(person)}`);
console.log(`Values: ${Object.values(person)}`);
console.log(`Entries: ${JSON.stringify(Object.entries(person))}`);

person.email = 'john@example.com';
console.log(`After adding email: ${JSON.stringify(person, null, 2)}`);

if ('name' in person) {
    console.log(`Name exists: ${person.name}`);
}

for (const [key, value] of Object.entries(person)) {
    console.log(`  ${key}: ${value}`);
}

const copied = { ...person, country: 'USA' };
console.log(`Copied with spread: ${JSON.stringify(copied, null, 2)}`);
