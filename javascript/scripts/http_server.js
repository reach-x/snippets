#!/usr/bin/env node

const http = require('http');
const url = require('url');

const PORT = process.env.PORT || 3000;

const server = http.createServer((req, res) => {
    const parsedUrl = url.parse(req.url, true);
    const pathname = parsedUrl.pathname;
    const method = req.method;

    console.log(`${method} ${pathname}`);

    res.setHeader('Content-Type', 'application/json');

    if (pathname === '/' && method === 'GET') {
        res.statusCode = 200;
        res.end(JSON.stringify({
            message: 'Welcome to the API',
            endpoints: ['/api/status', '/api/time', '/api/echo']
        }, null, 2));
    }
    else if (pathname === '/api/status' && method === 'GET') {
        res.statusCode = 200;
        res.end(JSON.stringify({
            status: 'ok',
            uptime: process.uptime(),
            timestamp: new Date().toISOString()
        }, null, 2));
    }
    else if (pathname === '/api/time' && method === 'GET') {
        res.statusCode = 200;
        res.end(JSON.stringify({
            time: new Date().toISOString(),
            unix: Date.now()
        }, null, 2));
    }
    else if (pathname === '/api/echo' && method === 'POST') {
        let body = '';
        req.on('data', chunk => body += chunk);
        req.on('end', () => {
            res.statusCode = 200;
            res.end(JSON.stringify({
                received: body,
                length: body.length
            }, null, 2));
        });
    }
    else {
        res.statusCode = 404;
        res.end(JSON.stringify({ error: 'Not found' }, null, 2));
    }
});

server.listen(PORT, () => {
    console.log(`Server running at http://localhost:${PORT}/`);
    console.log(`Try: curl http://localhost:${PORT}/api/status`);
});
