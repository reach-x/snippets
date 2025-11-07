#!/usr/bin/env node

function delay(ms) {
    return new Promise(resolve => setTimeout(resolve, ms));
}

async function asyncExample() {
    console.log('Starting async operation...');

    await delay(1000);
    console.log('After 1 second');

    await delay(500);
    console.log('After 1.5 seconds');

    return 'Async operation complete';
}

async function fetchExample() {
    console.log('\nFetching data...');

    try {
        const https = require('https');

        return new Promise((resolve, reject) => {
            https.get('https://api.github.com/users/github', {
                headers: { 'User-Agent': 'Node.js' }
            }, (res) => {
                let data = '';

                res.on('data', chunk => data += chunk);
                res.on('end', () => {
                    const parsed = JSON.parse(data);
                    console.log(`User: ${parsed.name}`);
                    console.log(`Public repos: ${parsed.public_repos}`);
                    resolve(parsed);
                });
            }).on('error', reject);
        });
    } catch (error) {
        console.error('Error:', error.message);
    }
}

async function runAll() {
    await asyncExample();
    await fetchExample();
}

runAll().then(() => console.log('\nAll operations complete'));
