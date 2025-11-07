#!/usr/bin/env node

const fs = require('fs');
const path = require('path');

const testFile = path.join(__dirname, '../tmp/test_output.txt');

const data = 'Hello from JavaScript!\nThis is a test file.\n';
fs.writeFileSync(testFile, data);
console.log(`Written to ${testFile}`);

const content = fs.readFileSync(testFile, 'utf8');
console.log(`Read content:\n${content}`);

const lines = content.split('\n').filter(line => line.length > 0);
console.log(`Lines: ${JSON.stringify(lines)}`);

if (fs.existsSync(testFile)) {
    const stats = fs.statSync(testFile);
    console.log(`File exists: ${testFile}`);
    console.log(`File size: ${stats.size} bytes`);
}

fs.appendFileSync(testFile, 'Appended line\n');
console.log(`Appended to file`);
