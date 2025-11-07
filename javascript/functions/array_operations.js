#!/usr/bin/env node

const numbers = [1, 2, 3, 4, 5];
const fruits = ['apple', 'banana', 'cherry'];

console.log(`Numbers: ${numbers}`);
console.log(`Length: ${numbers.length}`);

const sum = numbers.reduce((a, b) => a + b, 0);
console.log(`Sum: ${sum}`);

const max = Math.max(...numbers);
console.log(`Max: ${max}`);

const min = Math.min(...numbers);
console.log(`Min: ${min}`);

numbers.push(6);
console.log(`After push: ${numbers}`);

numbers.pop();
console.log(`After pop: ${numbers}`);

const squared = numbers.map(x => x ** 2);
console.log(`Squared: ${squared}`);

const filtered = numbers.filter(x => x % 2 === 0);
console.log(`Even numbers: ${filtered}`);

const combined = [...numbers, ...fruits];
console.log(`Combined: ${combined}`);

const sorted = fruits.sort();
console.log(`Sorted fruits: ${sorted}`);
