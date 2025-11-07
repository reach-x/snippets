#!/usr/bin/env python3

import os

test_file = '../tmp/test_output.txt'

data = "Hello from Python!\nThis is a test file.\n"
with open(test_file, 'w') as file_handle:
    file_handle.write(data)
print(f"Written to {test_file}")

with open(test_file, 'r') as file_handle:
    content = file_handle.read()
print(f"Read content:\n{content}")

with open(test_file, 'r') as file_handle:
    lines = file_handle.readlines()
print(f"Lines: {lines}")

if os.path.exists(test_file):
    print(f"File exists: {test_file}")
    print(f"File size: {os.path.getsize(test_file)} bytes")
