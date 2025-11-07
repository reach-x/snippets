const numbers: number[] = [1, 2, 3, 4, 5];
const fruits: string[] = ['apple', 'banana', 'cherry'];

console.log(`Numbers: ${numbers}`);
console.log(`Length: ${numbers.length}`);

const sum: number = numbers.reduce((a, b) => a + b, 0);
console.log(`Sum: ${sum}`);

const max: number = Math.max(...numbers);
console.log(`Max: ${max}`);

const min: number = Math.min(...numbers);
console.log(`Min: ${min}`);

numbers.push(6);
console.log(`After push: ${numbers}`);

numbers.pop();
console.log(`After pop: ${numbers}`);

const squared: number[] = numbers.map((x) => x ** 2);
console.log(`Squared: ${squared}`);

const filtered: number[] = numbers.filter((x) => x % 2 === 0);
console.log(`Even numbers: ${filtered}`);

const combined: (number | string)[] = [...numbers, ...fruits];
console.log(`Combined: ${combined}`);
