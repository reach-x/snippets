#!/usr/bin/env python3

import csv
import sys

def process_csv(input_file, output_file):
    data = []

    with open(input_file, 'r') as csv_file:
        reader = csv.DictReader(csv_file)
        for row in reader:
            data.append(row)

    print(f"Read {len(data)} rows from {input_file}")

    for index, row in enumerate(data[:5], 1):
        print(f"Row {index}: {row}")

    with open(output_file, 'w', newline='') as csv_file:
        if data:
            writer = csv.DictWriter(csv_file, fieldnames=data[0].keys())
            writer.writeheader()
            writer.writerows(data)
            print(f"\nWrote {len(data)} rows to {output_file}")

def create_sample_csv(filename):
    sample_data = [
        {'name': 'Alice', 'age': '30', 'city': 'New York'},
        {'name': 'Bob', 'age': '25', 'city': 'Los Angeles'},
        {'name': 'Charlie', 'age': '35', 'city': 'Chicago'},
    ]

    with open(filename, 'w', newline='') as csv_file:
        writer = csv.DictWriter(csv_file, fieldnames=['name', 'age', 'city'])
        writer.writeheader()
        writer.writerows(sample_data)
    print(f"Created sample CSV: {filename}")

if __name__ == '__main__':
    input_csv = '../tmp/sample.csv'
    output_csv = '../tmp/output.csv'

    create_sample_csv(input_csv)
    process_csv(input_csv, output_csv)
