#!/usr/bin/env python3

try:
    import requests

    url = 'https://api.github.com/users/github'

    response = requests.get(url)
    print(f"Status Code: {response.status_code}")
    print(f"Headers: {dict(response.headers)}")

    if response.status_code == 200:
        data = response.json()
        print(f"\nUser: {data.get('name')}")
        print(f"Bio: {data.get('bio')}")
        print(f"Public repos: {data.get('public_repos')}")

    post_data = {'key': 'value', 'name': 'test'}
    headers = {'Content-Type': 'application/json'}
    print(f"\nPOST data example prepared: {post_data}")

except ImportError:
    print("requests library not installed")
    print("Install with: pip install requests")
