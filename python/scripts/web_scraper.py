#!/usr/bin/env python3

import sys

try:
    import requests
    from bs4 import BeautifulSoup
except ImportError:
    print("Required libraries not installed")
    print("Install with: pip install requests beautifulsoup4")
    sys.exit(1)

def scrape_page(url):
    response = requests.get(url)
    soup = BeautifulSoup(response.content, 'html.parser')

    print(f"Title: {soup.title.string if soup.title else 'No title'}")
    print(f"\nLinks found:")

    links = soup.find_all('a', href=True)
    for index, link in enumerate(links[:10], 1):
        print(f"  {index}. {link.get('href')} - {link.text.strip()[:50]}")

    return soup

if __name__ == '__main__':
    url = sys.argv[1] if len(sys.argv) > 1 else 'https://github.com'
    print(f"Scraping: {url}\n")
    scrape_page(url)
