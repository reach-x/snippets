"""
aiohttp - Async HTTP client/server
Install: pip install aiohttp

Async HTTP client and server framework with WebSocket support
"""

import aiohttp
import asyncio
from aiohttp import web


# ===== CLIENT EXAMPLES =====

async def basic_get_request():
    """Simple async GET request"""
    async with aiohttp.ClientSession() as session:
        async with session.get('https://api.github.com') as response:
            print(f"Status: {response.status}")
            data = await response.json()
            print(f"Response keys: {list(data.keys())}")
            return data


async def multiple_requests():
    """Make multiple concurrent requests"""
    async with aiohttp.ClientSession() as session:
        urls = [
            'https://api.github.com/users/github',
            'https://api.github.com/users/python',
            'https://api.github.com/users/microsoft'
        ]

        tasks = [session.get(url) for url in urls]
        responses = await asyncio.gather(*tasks)

        for response in responses:
            data = await response.json()
            print(f"User: {data['login']}, Repos: {data['public_repos']}")


async def post_request_with_json():
    """POST request with JSON data"""
    async with aiohttp.ClientSession() as session:
        data = {'name': 'John', 'email': 'john@example.com'}

        async with session.post('https://httpbin.org/post', json=data) as response:
            result = await response.json()
            print(f"Posted data: {result['json']}")
            return result


async def request_with_headers():
    """Request with custom headers"""
    headers = {
        'User-Agent': 'My aiohttp App/1.0',
        'Authorization': 'Bearer token123'
    }

    async with aiohttp.ClientSession(headers=headers) as session:
        async with session.get('https://httpbin.org/headers') as response:
            data = await response.json()
            print(f"Headers sent: {data['headers']}")


async def request_with_timeout():
    """Request with timeout"""
    timeout = aiohttp.ClientTimeout(total=10, connect=5)

    async with aiohttp.ClientSession(timeout=timeout) as session:
        try:
            async with session.get('https://httpbin.org/delay/2') as response:
                print(f"Request completed: {response.status}")
        except asyncio.TimeoutError:
            print("Request timed out")


async def streaming_response():
    """Stream large response"""
    async with aiohttp.ClientSession() as session:
        async with session.get('https://httpbin.org/stream/5') as response:
            async for line in response.content:
                print(f"Chunk: {line[:50]}...")


async def download_file():
    """Download file with progress"""
    url = 'https://httpbin.org/image/png'

    async with aiohttp.ClientSession() as session:
        async with session.get(url) as response:
            with open('/tmp/downloaded_image.png', 'wb') as file:
                async for chunk in response.content.iter_chunked(8192):
                    file.write(chunk)

            print("File downloaded successfully")


async def session_with_connector():
    """Session with custom connector for connection pooling"""
    connector = aiohttp.TCPConnector(
        limit=100,  # Total connection limit
        limit_per_host=30,  # Per-host limit
        ttl_dns_cache=300  # DNS cache TTL
    )

    async with aiohttp.ClientSession(connector=connector) as session:
        async with session.get('https://api.github.com') as response:
            print(f"Status with custom connector: {response.status}")


# ===== SERVER EXAMPLES =====

# Simple route handler
async def handle_home(request):
    """Home route"""
    return web.Response(text="Hello from aiohttp server!")


async def handle_json(request):
    """JSON response"""
    data = {
        'message': 'Hello from aiohttp',
        'status': 'success'
    }
    return web.json_response(data)


async def handle_user_get(request):
    """GET route with path parameter"""
    user_id = request.match_info['user_id']
    return web.json_response({
        'user_id': user_id,
        'name': f'User {user_id}'
    })


async def handle_user_post(request):
    """POST route with JSON body"""
    data = await request.json()

    return web.json_response({
        'message': 'User created',
        'data': data
    }, status=201)


async def handle_query_params(request):
    """Handle query parameters"""
    name = request.query.get('name', 'Guest')
    age = request.query.get('age', type=int)

    return web.json_response({
        'name': name,
        'age': age
    })


async def handle_upload(request):
    """Handle file upload"""
    reader = await request.multipart()

    field = await reader.next()
    filename = field.filename
    size = 0

    while True:
        chunk = await field.read_chunk()
        if not chunk:
            break
        size += len(chunk)

    return web.json_response({
        'filename': filename,
        'size': size
    })


# Middleware
@web.middleware
async def error_middleware(request, handler):
    """Error handling middleware"""
    try:
        response = await handler(request)
        return response
    except web.HTTPException as ex:
        return web.json_response({
            'error': ex.reason
        }, status=ex.status)
    except Exception as ex:
        return web.json_response({
            'error': str(ex)
        }, status=500)


@web.middleware
async def logging_middleware(request, handler):
    """Logging middleware"""
    print(f"Request: {request.method} {request.path}")
    response = await handler(request)
    print(f"Response: {response.status}")
    return response


# Application lifecycle
async def on_startup(app):
    """Run on application startup"""
    print("aiohttp server starting up...")


async def on_cleanup(app):
    """Run on application cleanup"""
    print("aiohttp server cleaning up...")


# WebSocket handler
async def websocket_handler(request):
    """WebSocket endpoint"""
    websocket = web.WebSocketResponse()
    await websocket.prepare(request)

    async for message in websocket:
        if message.type == aiohttp.WSMsgType.TEXT:
            if message.data == 'close':
                await websocket.close()
            else:
                # Echo back the message
                await websocket.send_str(f"Echo: {message.data}")
        elif message.type == aiohttp.WSMsgType.ERROR:
            print(f"WebSocket error: {websocket.exception()}")

    return websocket


def create_app():
    """Create and configure the application"""
    app = web.Application(middlewares=[logging_middleware, error_middleware])

    # Add routes
    app.router.add_get('/', handle_home)
    app.router.add_get('/api/hello', handle_json)
    app.router.add_get('/api/users/{user_id}', handle_user_get)
    app.router.add_post('/api/users', handle_user_post)
    app.router.add_get('/api/search', handle_query_params)
    app.router.add_post('/api/upload', handle_upload)
    app.router.add_get('/ws', websocket_handler)

    # Add startup/cleanup handlers
    app.on_startup.append(on_startup)
    app.on_cleanup.append(on_cleanup)

    return app


# ===== CLIENT SESSION WITH COOKIE JAR =====

async def session_with_cookies():
    """Session that persists cookies"""
    jar = aiohttp.CookieJar()

    async with aiohttp.ClientSession(cookie_jar=jar) as session:
        # First request sets cookies
        await session.get('https://httpbin.org/cookies/set/session/12345')

        # Second request includes cookies
        async with session.get('https://httpbin.org/cookies') as response:
            data = await response.json()
            print(f"Cookies: {data['cookies']}")


if __name__ == "__main__":
    print("=== aiohttp Client Examples ===\n")

    print("1. Basic GET Request")
    asyncio.run(basic_get_request())

    print("\n2. Multiple Concurrent Requests")
    asyncio.run(multiple_requests())

    print("\n3. POST Request with JSON")
    asyncio.run(post_request_with_json())

    print("\n4. Request with Headers")
    asyncio.run(request_with_headers())

    print("\n5. Request with Timeout")
    asyncio.run(request_with_timeout())

    print("\n6. Streaming Response")
    asyncio.run(streaming_response())

    print("\n7. Download File")
    asyncio.run(download_file())

    print("\n8. Session with Cookies")
    asyncio.run(session_with_cookies())

    print("\n\n=== aiohttp Server Example ===")
    print("To run the server:")
    print("  python -c 'from aiohttp_example import create_app; from aiohttp import web; web.run_app(create_app())'")
    print("\nServer routes:")
    print("  GET  /                  - Home")
    print("  GET  /api/hello         - JSON response")
    print("  GET  /api/users/{id}    - Get user")
    print("  POST /api/users         - Create user")
    print("  GET  /api/search?name=  - Query params")
    print("  POST /api/upload        - File upload")
    print("  GET  /ws                - WebSocket")
