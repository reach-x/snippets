#!/usr/bin/env python3

import json

data = {
    'name': 'John Doe',
    'age': 30,
    'skills': ['Python', 'JavaScript', 'SQL'],
    'active': True
}

json_string = json.dumps(data, indent=2)
print(f"JSON String:\n{json_string}")

parsed_data = json.loads(json_string)
print(f"\nParsed data: {parsed_data}")
print(f"Name: {parsed_data['name']}")
print(f"Skills: {', '.join(parsed_data['skills'])}")

json_file = '../tmp/data.json'
with open(json_file, 'w') as file_handle:
    json.dump(data, file_handle, indent=2)
print(f"\nWritten to {json_file}")

with open(json_file, 'r') as file_handle:
    loaded_data = json.load(file_handle)
print(f"Loaded from file: {loaded_data}")
