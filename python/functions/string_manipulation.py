#!/usr/bin/env python3

text = "  Hello, World!  "

print(f"Original: '{text}'")
print(f"Upper: '{text.upper()}'")
print(f"Lower: '{text.lower()}'")
print(f"Strip: '{text.strip()}'")
print(f"Replace: '{text.replace('World', 'Python')}'")
print(f"Split: {text.strip().split(', ')}")
print(f"StartsWith: {text.strip().startswith('Hello')}")
print(f"EndsWith: {text.strip().endswith('!')}")
