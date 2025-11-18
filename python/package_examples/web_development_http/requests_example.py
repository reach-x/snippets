"""
requests - HTTP library for humans
Install: pip install requests

Simple, elegant HTTP library for making requests
"""

import requests
from requests.auth import HTTPBasicAuth


def basic_get_request():
    """Simple GET request"""
    response = requests.get('https://api.github.com')
    print(f"Status Code: {response.status_code}")
    print(f"Content-Type: {response.headers['Content-Type']}")
    print(f"Response JSON: {response.json()}")
    return response


def get_with_parameters():
    """GET request with query parameters"""
    parameters = {'q': 'requests', 'sort': 'stars'}
    response = requests.get('https://api.github.com/search/repositories', params=parameters)

    data = response.json()
    print(f"Total repositories found: {data['total_count']}")
    print(f"First repo: {data['items'][0]['full_name']}")
    return response


def post_request_with_json():
    """POST request with JSON data"""
    url = 'https://httpbin.org/post'
    data = {
        'name': 'John Doe',
        'email': 'john@example.com',
        'age': 30
    }

    response = requests.post(url, json=data)
    print(f"Posted data: {response.json()['json']}")
    return response


def request_with_headers():
    """Request with custom headers"""
    headers = {
        'User-Agent': 'My Python App/1.0',
        'Accept': 'application/json'
    }

    response = requests.get('https://api.github.com/users/github', headers=headers)
    print(f"User: {response.json()['name']}")
    return response


def request_with_timeout():
    """Request with timeout"""
    try:
        response = requests.get('https://httpbin.org/delay/2', timeout=5)
        print(f"Request completed in time: {response.status_code}")
    except requests.Timeout:
        print("Request timed out")
    return response


def request_with_authentication():
    """Request with basic authentication"""
    response = requests.get(
        'https://httpbin.org/basic-auth/user/pass',
        auth=HTTPBasicAuth('user', 'pass')
    )
    print(f"Authenticated: {response.status_code}")
    return response


def session_example():
    """Using sessions for persistent parameters"""
    session = requests.Session()
    session.headers.update({'User-Agent': 'My App'})

    # All requests using this session will have the User-Agent header
    response1 = session.get('https://httpbin.org/headers')
    response2 = session.get('https://httpbin.org/user-agent')

    print(f"Session headers: {response1.json()}")
    return session


def download_file():
    """Download a file"""
    url = 'https://httpbin.org/image/png'
    response = requests.get(url, stream=True)

    with open('/tmp/downloaded_image.png', 'wb') as file:
        for chunk in response.iter_content(chunk_size=8192):
            file.write(chunk)

    print("File downloaded successfully")
    return response


def handle_errors():
    """Proper error handling"""
    try:
        response = requests.get('https://api.github.com/invalid-endpoint')
        response.raise_for_status()  # Raises HTTPError for bad status codes
    except requests.HTTPError as http_error:
        print(f"HTTP error occurred: {http_error}")
    except requests.ConnectionError:
        print("Connection error occurred")
    except requests.Timeout:
        print("Timeout error occurred")
    except requests.RequestException as error:
        print(f"Error occurred: {error}")


if __name__ == "__main__":
    print("=== Basic GET Request ===")
    basic_get_request()

    print("\n=== GET with Parameters ===")
    get_with_parameters()

    print("\n=== POST with JSON ===")
    post_request_with_json()

    print("\n=== Request with Headers ===")
    request_with_headers()

    print("\n=== Request with Timeout ===")
    request_with_timeout()

    print("\n=== Request with Authentication ===")
    request_with_authentication()

    print("\n=== Session Example ===")
    session_example()

    print("\n=== Download File ===")
    download_file()

    print("\n=== Error Handling ===")
    handle_errors()
