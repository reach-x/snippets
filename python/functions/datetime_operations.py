#!/usr/bin/env python3

from datetime import datetime, timedelta

now = datetime.now()
print(f"Current datetime: {now}")
print(f"Formatted: {now.strftime('%Y-%m-%d %H:%M:%S')}")
print(f"Date only: {now.strftime('%Y-%m-%d')}")
print(f"Time only: {now.strftime('%H:%M:%S')}")

yesterday = now - timedelta(days=1)
print(f"Yesterday: {yesterday.strftime('%Y-%m-%d')}")

next_week = now + timedelta(weeks=1)
print(f"Next week: {next_week.strftime('%Y-%m-%d')}")

date_string = "2025-01-15 14:30:00"
parsed_date = datetime.strptime(date_string, '%Y-%m-%d %H:%M:%S')
print(f"Parsed date: {parsed_date}")

timestamp = now.timestamp()
print(f"Timestamp: {timestamp}")
