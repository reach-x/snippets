# JavaScript Quick Reference

## Basic Syntax
```javascript
// Variables
let name = 'Alice';
const age = 30;
var oldStyle = 'avoid var';  // Use let/const instead

// Comments
// Single line comment
/*
 Multi-line comment
*/
```

## Data Types
```javascript
// String
const text = "Hello, World!";
const template = `Name: ${name}, Age: ${age}`;

// Number
const integer = 42;
const floating = 3.14;

// Boolean
const isActive = true;

// Null and undefined
const nothing = null;
let notDefined;  // undefined

// Symbol
const sym = Symbol('description');

// BigInt
const bigNum = 1234567890123456789012345678901234567890n;
```

## Collections
```javascript
// Array
const numbers = [1, 2, 3, 4, 5];
numbers.push(6);
numbers.pop();

// Object
const person = {
    name: 'John',
    age: 30,
    email: 'john@example.com'
};

// Map
const map = new Map();
map.set('key', 'value');

// Set
const uniqueNumbers = new Set([1, 2, 3, 3, 4]);  // {1, 2, 3, 4}
```

## Control Flow
```javascript
// If/else
if (age >= 18) {
    console.log('Adult');
} else if (age >= 13) {
    console.log('Teenager');
} else {
    console.log('Child');
}

// Ternary operator
const status = age >= 18 ? 'Adult' : 'Minor';

// For loop
for (let i = 0; i < 5; i++) {
    console.log(i);
}

// For...of (values)
for (const item of [1, 2, 3]) {
    console.log(item);
}

// For...in (keys)
for (const key in obj) {
    console.log(key, obj[key]);
}

// While loop
let count = 0;
while (count < 5) {
    console.log(count);
    count++;
}
```

## Functions
```javascript
// Function declaration
function greet(name) {
    return `Hello, ${name}!`;
}

// Function expression
const greet = function(name) {
    return `Hello, ${name}!`;
};

// Arrow function
const greet = (name) => `Hello, ${name}!`;
const square = x => x ** 2;  // Single param, implicit return

// Default parameters
function power(base, exponent = 2) {
    return base ** exponent;
}

// Rest parameters
function sum(...numbers) {
    return numbers.reduce((a, b) => a + b, 0);
}

// Destructuring
const {name, age} = person;
const [first, second] = array;
```

## Classes
```javascript
class Person {
    constructor(name, age) {
        this.name = name;
        this.age = age;
    }

    greet() {
        return `Hello, I'm ${this.name}`;
    }

    static create(name, age) {
        return new Person(name, age);
    }
}

// Inheritance
class Employee extends Person {
    constructor(name, age, jobTitle) {
        super(name, age);
        this.jobTitle = jobTitle;
    }
}
```

## Async/Await
```javascript
// Promise
fetch('https://api.example.com/data')
    .then(response => response.json())
    .then(data => console.log(data))
    .catch(error => console.error(error));

// Async/await
async function fetchData() {
    try {
        const response = await fetch('https://api.example.com/data');
        const data = await response.json();
        return data;
    } catch (error) {
        console.error(error);
    }
}
```

## Array Methods
```javascript
const numbers = [1, 2, 3, 4, 5];

numbers.map(x => x * 2);         // [2, 4, 6, 8, 10]
numbers.filter(x => x % 2 === 0); // [2, 4]
numbers.reduce((a, b) => a + b);  // 15
numbers.find(x => x > 3);         // 4
numbers.some(x => x > 3);         // true
numbers.every(x => x > 0);        // true
numbers.includes(3);              // true
```

## Common Operations
```javascript
// String manipulation
text.toUpperCase(), text.toLowerCase(), text.trim()
text.split(','), array.join(',')
text.replace('old', 'new')
text.includes('substring')

// Object operations
Object.keys(obj), Object.values(obj), Object.entries(obj)
Object.assign({}, obj)
{...obj, newKey: 'value'}  // Spread operator

// Array operations
array.length, array.push(), array.pop()
array.slice(), array.splice()
[...array]  // Spread operator
```

## Modules (ES6)
```javascript
// Export
export const myFunction = () => {};
export default class MyClass {}

// Import
import MyClass from './module';
import { myFunction } from './module';
import * as utils from './utils';
```

## Common Patterns
```javascript
// Optional chaining
const value = obj?.prop?.nestedProp;

// Nullish coalescing
const value = variable ?? 'default';

// Template literals
const message = `Hello, ${name}!`;

// Destructuring with default
const {name = 'Unknown', age = 0} = person;
```

## Tips
- Use `const` by default, `let` when reassignment is needed
- Use `camelCase` for variables and functions
- Use `PascalCase` for classes
- Use arrow functions for callbacks
- Use async/await instead of promise chains
- Use strict mode: `'use strict';`
