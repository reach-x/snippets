function delay(ms: number): Promise<void> {
    return new Promise(resolve => setTimeout(resolve, ms));
}

async function asyncExample(): Promise<string> {
    console.log('Starting async operation...');

    await delay(1000);
    console.log('After 1 second');

    await delay(500);
    console.log('After 1.5 seconds');

    return 'Async operation complete';
}

interface GitHubUser {
    name: string;
    bio: string;
    public_repos: number;
}

async function fetchExample(): Promise<void> {
    console.log('\nFetching data...');

    try {
        // Note: In a real TypeScript project, you would use node-fetch or axios
        const https = require('https');

        return new Promise((resolve, reject) => {
            https.get('https://api.github.com/users/github', {
                headers: { 'User-Agent': 'TypeScript' }
            }, (res: any) => {
                let data = '';

                res.on('data', (chunk: string) => data += chunk);
                res.on('end', () => {
                    const parsed: GitHubUser = JSON.parse(data);
                    console.log(`User: ${parsed.name}`);
                    console.log(`Public repos: ${parsed.public_repos}`);
                    resolve();
                });
            }).on('error', reject);
        });
    } catch (error) {
        console.error('Error:', (error as Error).message);
    }
}

async function runAll(): Promise<void> {
    await asyncExample();
    await fetchExample();
}

runAll().then(() => console.log('\nAll operations complete'));
