"""
httpx - Modern HTTP client with async support
Install: pip install httpx

Async HTTP client with same API as requests, plus HTTP/2 support
"""

import httpx
import asyncio


def sync_get_request():
    """Simple synchronous GET request"""
    response = httpx.get('https://api.github.com')
    print(f"Status Code: {response.status_code}")
    print(f"HTTP Version: {response.http_version}")
    print(f"Response JSON keys: {list(response.json().keys())}")
    return response


def sync_client_example():
    """Using synchronous client for connection pooling"""
    with httpx.Client() as client:
        response1 = client.get('https://api.github.com/users/github')
        response2 = client.get('https://api.github.com/users/python')

        print(f"User 1: {response1.json()['name']}")
        print(f"User 2: {response2.json()['name']}")


async def async_get_request():
    """Simple asynchronous GET request"""
    async with httpx.AsyncClient() as client:
        response = await client.get('https://api.github.com')
        print(f"Async Status Code: {response.status_code}")
        return response


async def multiple_async_requests():
    """Making multiple async requests concurrently"""
    async with httpx.AsyncClient() as client:
        # Create list of coroutines
        requests = [
            client.get('https://api.github.com/users/github'),
            client.get('https://api.github.com/users/python'),
            client.get('https://api.github.com/users/microsoft')
        ]

        # Execute all requests concurrently
        responses = await asyncio.gather(*requests)

        for response in responses:
            user_data = response.json()
            print(f"User: {user_data['login']}, Followers: {user_data['followers']}")


async def async_post_request():
    """Async POST request with JSON data"""
    async with httpx.AsyncClient() as client:
        data = {'name': 'John', 'email': 'john@example.com'}
        response = await client.post('https://httpbin.org/post', json=data)
        print(f"Posted data: {response.json()['json']}")
        return response


async def streaming_response():
    """Stream large response"""
    async with httpx.AsyncClient() as client:
        async with client.stream('GET', 'https://httpbin.org/stream/10') as response:
            async for line in response.aiter_lines():
                print(f"Streamed line: {line[:50]}...")  # First 50 chars


def http2_example():
    """Using HTTP/2"""
    with httpx.Client(http2=True) as client:
        response = client.get('https://www.google.com')
        print(f"HTTP Version: {response.http_version}")
        print(f"Status: {response.status_code}")


def timeout_and_limits():
    """Configure timeout and connection limits"""
    timeout = httpx.Timeout(10.0, connect=5.0)
    limits = httpx.Limits(max_keepalive_connections=5, max_connections=10)

    with httpx.Client(timeout=timeout, limits=limits) as client:
        response = client.get('https://httpbin.org/delay/2')
        print(f"Response with custom timeout: {response.status_code}")


async def advanced_async_client():
    """Advanced async client with custom configuration"""
    timeout = httpx.Timeout(10.0, connect=5.0)
    headers = {'User-Agent': 'My Async App/1.0'}

    async with httpx.AsyncClient(timeout=timeout, headers=headers) as client:
        response = await client.get('https://httpbin.org/headers')
        print(f"Custom headers used: {response.json()['headers']['User-Agent']}")


def event_hooks_example():
    """Using event hooks to log requests and responses"""
    def log_request(request):
        print(f"Request: {request.method} {request.url}")

    def log_response(response):
        print(f"Response: {response.status_code}")

    client = httpx.Client(
        event_hooks={'request': [log_request], 'response': [log_response]}
    )

    with client:
        response = client.get('https://httpbin.org/get')
        print("Event hooks executed")


if __name__ == "__main__":
    print("=== Sync GET Request ===")
    sync_get_request()

    print("\n=== Sync Client Example ===")
    sync_client_example()

    print("\n=== Async GET Request ===")
    asyncio.run(async_get_request())

    print("\n=== Multiple Async Requests ===")
    asyncio.run(multiple_async_requests())

    print("\n=== Async POST Request ===")
    asyncio.run(async_post_request())

    print("\n=== Streaming Response ===")
    asyncio.run(streaming_response())

    print("\n=== HTTP/2 Example ===")
    http2_example()

    print("\n=== Timeout and Limits ===")
    timeout_and_limits()

    print("\n=== Advanced Async Client ===")
    asyncio.run(advanced_async_client())

    print("\n=== Event Hooks Example ===")
    event_hooks_example()
