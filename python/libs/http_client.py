#!/usr/bin/env python3

import json

class HttpClient:
    def __init__(self, base_url='', default_headers=None):
        try:
            import requests
            self.requests = requests
        except ImportError:
            raise ImportError("requests library required: pip install requests")

        self.base_url = base_url.rstrip('/')
        self.default_headers = default_headers or {}
        self.session = self.requests.Session()

    def get(self, endpoint, params=None, headers=None):
        url = f"{self.base_url}{endpoint}" if self.base_url else endpoint
        merged_headers = {**self.default_headers, **(headers or {})}

        response = self.session.get(url, params=params, headers=merged_headers)
        return self._process_response(response)

    def post(self, endpoint, data=None, json_data=None, headers=None):
        url = f"{self.base_url}{endpoint}" if self.base_url else endpoint
        merged_headers = {**self.default_headers, **(headers or {})}

        response = self.session.post(url, data=data, json=json_data, headers=merged_headers)
        return self._process_response(response)

    def put(self, endpoint, data=None, json_data=None, headers=None):
        url = f"{self.base_url}{endpoint}" if self.base_url else endpoint
        merged_headers = {**self.default_headers, **(headers or {})}

        response = self.session.put(url, data=data, json=json_data, headers=merged_headers)
        return self._process_response(response)

    def delete(self, endpoint, headers=None):
        url = f"{self.base_url}{endpoint}" if self.base_url else endpoint
        merged_headers = {**self.default_headers, **(headers or {})}

        response = self.session.delete(url, headers=merged_headers)
        return self._process_response(response)

    def _process_response(self, response):
        result = {
            'status_code': response.status_code,
            'headers': dict(response.headers),
            'content': response.text,
        }

        try:
            result['json'] = response.json()
        except:
            result['json'] = None

        return result

if __name__ == '__main__':
    client = HttpClient(base_url='https://api.github.com')

    result = client.get('/users/github')
    print(f"Status: {result['status_code']}")
    if result['json']:
        print(f"User: {result['json'].get('name')}")
        print(f"Repos: {result['json'].get('public_repos')}")
