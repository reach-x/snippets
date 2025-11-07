const text: string = "  Hello, World!  ";

console.log(`Original: '${text}'`);
console.log(`Upper: '${text.toUpperCase()}'`);
console.log(`Lower: '${text.toLowerCase()}'`);
console.log(`Trim: '${text.trim()}'`);
console.log(`Replace: '${text.replace('World', 'TypeScript')}'`);
console.log(`Split: ${text.trim().split(', ')}`);
console.log(`StartsWith: ${text.trim().startsWith('Hello')}`);
console.log(`EndsWith: ${text.trim().endsWith('!')}`);
console.log(`Includes: ${text.includes('World')}`);
console.log(`Substring: '${text.trim().substring(0, 5)}'`);
console.log(`Repeat: '${'-'.repeat(20)}'`);
