#!/usr/bin/env python3

person = {
    'name': 'John',
    'age': 30,
    'city': 'New York'
}

print(f"Person: {person}")
print(f"Name: {person['name']}")
print(f"Age: {person.get('age')}")
print(f"Keys: {list(person.keys())}")
print(f"Values: {list(person.values())}")
print(f"Items: {list(person.items())}")

person['email'] = 'john@example.com'
print(f"After adding email: {person}")

if 'name' in person:
    print(f"Name exists: {person['name']}")

for key, value in person.items():
    print(f"  {key}: {value}")
