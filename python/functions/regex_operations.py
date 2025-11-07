#!/usr/bin/env python3

import re

text = "Contact us at support@example.com or sales@example.com"
phone = "Call 123-456-7890 or 987-654-3210"

email_pattern = r'\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Z|a-z]{2,}\b'
emails = re.findall(email_pattern, text)
print(f"Found emails: {emails}")

phone_pattern = r'\d{3}-\d{3}-\d{4}'
phones = re.findall(phone_pattern, phone)
print(f"Found phones: {phones}")

replaced = re.sub(r'example\.com', 'test.com', text)
print(f"Replaced: {replaced}")

if re.search(r'\d{3}-\d{3}-\d{4}', phone):
    print("Phone number format found")

parts = re.split(r'\s+or\s+', text)
print(f"Split parts: {parts}")
