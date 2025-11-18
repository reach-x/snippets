"""
urllib3 - HTTP client (lower level)
Install: pip install urllib3

Low-level HTTP client with connection pooling, retries, and redirects
"""

import urllib3
import json


# ===== Basic Requests =====

def basic_get_request():
    """Simple GET request"""
    http = urllib3.PoolManager()

    response = http.request('GET', 'https://httpbin.org/get')

    print(f"Status: {response.status}")
    print(f"Data: {response.data.decode('utf-8')[:100]}...")

    return response


def get_json_response():
    """GET request and parse JSON"""
    http = urllib3.PoolManager()

    response = http.request('GET', 'https://api.github.com')
    data = json.loads(response.data.decode('utf-8'))

    print(f"GitHub API keys: {list(data.keys())}")
    return data


# ===== POST Requests =====

def post_with_json():
    """POST request with JSON data"""
    http = urllib3.PoolManager()

    encoded_data = json.dumps({
        'name': 'John Doe',
        'email': 'john@example.com'
    }).encode('utf-8')

    response = http.request(
        'POST',
        'https://httpbin.org/post',
        body=encoded_data,
        headers={'Content-Type': 'application/json'}
    )

    print(f"Status: {response.status}")
    data = json.loads(response.data)
    print(f"Posted data: {data['json']}")

    return response


def post_with_form_data():
    """POST request with form-encoded data"""
    http = urllib3.PoolManager()

    response = http.request(
        'POST',
        'https://httpbin.org/post',
        fields={'name': 'John', 'email': 'john@example.com'}
    )

    print(f"Form data posted: {response.status}")
    return response


# ===== Headers =====

def request_with_headers():
    """Request with custom headers"""
    http = urllib3.PoolManager()

    headers = {
        'User-Agent': 'My urllib3 App/1.0',
        'Accept': 'application/json',
        'X-Custom-Header': 'custom-value'
    }

    response = http.request(
        'GET',
        'https://httpbin.org/headers',
        headers=headers
    )

    data = json.loads(response.data)
    print(f"Headers sent: {data['headers']}")

    return response


# ===== Connection Pooling =====

def connection_pooling_example():
    """Using connection pooling for multiple requests"""
    # Create a pool manager with custom settings
    http = urllib3.PoolManager(
        num_pools=10,
        maxsize=10,
        block=False
    )

    # Make multiple requests using the same pool
    urls = [
        'https://httpbin.org/get',
        'https://httpbin.org/user-agent',
        'https://httpbin.org/headers'
    ]

    for url in urls:
        response = http.request('GET', url)
        print(f"URL: {url}, Status: {response.status}")

    return http


# ===== Retries =====

def request_with_retries():
    """Request with automatic retries"""
    http = urllib3.PoolManager(
        retries=urllib3.Retry(
            total=3,
            backoff_factor=0.3,
            status_forcelist=[500, 502, 503, 504]
        )
    )

    try:
        response = http.request('GET', 'https://httpbin.org/status/500')
        print(f"Status: {response.status}")
    except urllib3.exceptions.MaxRetryError as error:
        print(f"Max retries exceeded: {error}")

    return http


def custom_retry_strategy():
    """Custom retry strategy"""
    retry_strategy = urllib3.Retry(
        total=5,  # Total number of retries
        connect=2,  # Connection retries
        read=2,  # Read retries
        redirect=3,  # Redirect retries
        backoff_factor=1,  # Wait 1, 2, 4, 8, 16 seconds between retries
        status_forcelist=[429, 500, 502, 503, 504],  # Retry on these status codes
        allowed_methods=["HEAD", "GET", "OPTIONS"]  # Only retry these methods
    )

    http = urllib3.PoolManager(retries=retry_strategy)

    response = http.request('GET', 'https://httpbin.org/get')
    print(f"Request with custom retry: {response.status}")

    return response


# ===== Timeouts =====

def request_with_timeout():
    """Request with timeout"""
    http = urllib3.PoolManager()

    try:
        response = http.request(
            'GET',
            'https://httpbin.org/delay/5',
            timeout=3.0  # 3 second timeout
        )
        print(f"Status: {response.status}")
    except urllib3.exceptions.TimeoutError:
        print("Request timed out")

    return http


def request_with_separate_timeouts():
    """Request with separate connect and read timeouts"""
    http = urllib3.PoolManager()

    timeout = urllib3.Timeout(connect=2.0, read=5.0)

    response = http.request(
        'GET',
        'https://httpbin.org/delay/2',
        timeout=timeout
    )

    print(f"Status with separate timeouts: {response.status}")
    return response


# ===== Streaming Responses =====

def streaming_response():
    """Stream large response"""
    http = urllib3.PoolManager()

    response = http.request(
        'GET',
        'https://httpbin.org/stream/10',
        preload_content=False
    )

    try:
        for line in response.stream(1024):
            print(f"Chunk: {line.decode('utf-8')[:50]}...")
    finally:
        response.release_conn()


# ===== File Upload =====

def upload_file():
    """Upload file"""
    http = urllib3.PoolManager()

    with open('/tmp/test_file.txt', 'w') as f:
        f.write('Hello from urllib3!')

    with open('/tmp/test_file.txt', 'rb') as file:
        response = http.request(
            'POST',
            'https://httpbin.org/post',
            fields={
                'file': ('test_file.txt', file.read(), 'text/plain')
            }
        )

    print(f"File upload status: {response.status}")
    return response


# ===== SSL/TLS Configuration =====

def disable_ssl_warnings():
    """Disable SSL warnings (not recommended for production)"""
    urllib3.disable_warnings(urllib3.exceptions.InsecureRequestWarning)

    http = urllib3.PoolManager()
    response = http.request('GET', 'https://httpbin.org/get')

    print(f"Request with disabled SSL warnings: {response.status}")
    return response


def custom_ssl_context():
    """Custom SSL context"""
    import ssl
    import certifi

    http = urllib3.PoolManager(
        cert_reqs=ssl.CERT_REQUIRED,
        ca_certs=certifi.where()
    )

    response = http.request('GET', 'https://httpbin.org/get')
    print(f"Request with custom SSL: {response.status}")

    return response


# ===== Proxy Support =====

def request_through_proxy():
    """Request through HTTP proxy"""
    # Example proxy configuration (will fail without actual proxy)
    # proxy = urllib3.ProxyManager('http://proxy.example.com:8080/')
    # response = proxy.request('GET', 'https://httpbin.org/ip')
    # print(f"IP through proxy: {response.data}")

    print("Proxy example (configure with actual proxy server)")


# ===== Response Headers =====

def inspect_response_headers():
    """Inspect response headers"""
    http = urllib3.PoolManager()

    response = http.request('GET', 'https://httpbin.org/response-headers')

    print(f"Response headers:")
    for header, value in response.headers.items():
        print(f"  {header}: {value}")

    return response


# ===== Query Parameters =====

def request_with_query_params():
    """Request with query parameters"""
    http = urllib3.PoolManager()

    response = http.request(
        'GET',
        'https://httpbin.org/get',
        fields={'name': 'John', 'age': '30'}
    )

    data = json.loads(response.data)
    print(f"Query params: {data['args']}")

    return response


if __name__ == "__main__":
    print("=== urllib3 Examples ===\n")

    print("1. Basic GET Request")
    basic_get_request()

    print("\n2. GET JSON Response")
    get_json_response()

    print("\n3. POST with JSON")
    post_with_json()

    print("\n4. POST with Form Data")
    post_with_form_data()

    print("\n5. Request with Headers")
    request_with_headers()

    print("\n6. Connection Pooling")
    connection_pooling_example()

    print("\n7. Request with Retries")
    request_with_retries()

    print("\n8. Custom Retry Strategy")
    custom_retry_strategy()

    print("\n9. Request with Timeout")
    request_with_timeout()

    print("\n10. Separate Connect/Read Timeouts")
    request_with_separate_timeouts()

    print("\n11. Streaming Response")
    streaming_response()

    print("\n12. Upload File")
    upload_file()

    print("\n13. Inspect Response Headers")
    inspect_response_headers()

    print("\n14. Query Parameters")
    request_with_query_params()

    print("\nKey Features:")
    print("  - Connection pooling for performance")
    print("  - Automatic retries with backoff")
    print("  - Timeout configuration")
    print("  - Streaming responses")
    print("  - SSL/TLS configuration")
    print("  - Proxy support")
