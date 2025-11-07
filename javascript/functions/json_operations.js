#!/usr/bin/env node

const fs = require('fs');
const path = require('path');

const data = {
    name: 'John Doe',
    age: 30,
    skills: ['JavaScript', 'Node.js', 'React'],
    active: true
};

const jsonString = JSON.stringify(data, null, 2);
console.log(`JSON String:\n${jsonString}`);

const parsedData = JSON.parse(jsonString);
console.log(`\nParsed data: ${JSON.stringify(parsedData)}`);
console.log(`Name: ${parsedData.name}`);
console.log(`Skills: ${parsedData.skills.join(', ')}`);

const jsonFile = path.join(__dirname, '../tmp/data.json');
fs.writeFileSync(jsonFile, jsonString);
console.log(`\nWritten to ${jsonFile}`);

const loadedData = JSON.parse(fs.readFileSync(jsonFile, 'utf8'));
console.log(`Loaded from file: ${JSON.stringify(loadedData)}`);
