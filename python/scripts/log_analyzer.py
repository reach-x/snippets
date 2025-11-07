#!/usr/bin/env python3

import re
from collections import Counter
from datetime import datetime

def analyze_log_file(log_file):
    ip_pattern = r'\b(?:[0-9]{1,3}\.){3}[0-9]{1,3}\b'
    status_pattern = r'" (\d{3}) '

    ip_addresses = []
    status_codes = []

    try:
        with open(log_file, 'r') as file_handle:
            for line in file_handle:
                ips = re.findall(ip_pattern, line)
                ip_addresses.extend(ips)

                statuses = re.findall(status_pattern, line)
                status_codes.extend(statuses)

        print(f"Log Analysis for: {log_file}")
        print(f"Total lines processed: {sum([1 for _ in open(log_file)])}")

        print(f"\nTop 10 IP Addresses:")
        for ip, count in Counter(ip_addresses).most_common(10):
            print(f"  {ip}: {count}")

        print(f"\nStatus Code Distribution:")
        for status, count in Counter(status_codes).most_common():
            print(f"  {status}: {count}")

    except FileNotFoundError:
        print(f"Creating sample log file: {log_file}")
        create_sample_log(log_file)
        analyze_log_file(log_file)

def create_sample_log(filename):
    sample_logs = [
        '192.168.1.1 - - [01/Jan/2025:12:00:00 +0000] "GET /index.html HTTP/1.1" 200 1234',
        '192.168.1.2 - - [01/Jan/2025:12:00:01 +0000] "GET /about.html HTTP/1.1" 200 5678',
        '192.168.1.1 - - [01/Jan/2025:12:00:02 +0000] "GET /contact.html HTTP/1.1" 404 0',
        '192.168.1.3 - - [01/Jan/2025:12:00:03 +0000] "POST /api/data HTTP/1.1" 201 890',
        '192.168.1.2 - - [01/Jan/2025:12:00:04 +0000] "GET /images/logo.png HTTP/1.1" 304 0',
    ]

    with open(filename, 'w') as file_handle:
        for log in sample_logs:
            file_handle.write(log + '\n')

if __name__ == '__main__':
    log_file = '../tmp/access.log'
    analyze_log_file(log_file)
