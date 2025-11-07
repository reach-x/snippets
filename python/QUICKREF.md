# Python Quick Reference

## Basic Syntax
```python
# Variables
name = "Alice"
age = 30
is_active = True

# Comments
# Single line comment
"""
Multi-line comment
or docstring
"""
```

## Data Types
```python
# String
text = "Hello, World!"
f_string = f"Name: {name}, Age: {age}"

# Numbers
integer = 42
floating = 3.14

# Boolean
is_valid = True

# None
value = None
```

## Collections
```python
# List (mutable, ordered)
numbers = [1, 2, 3, 4, 5]
numbers.append(6)
numbers.pop()

# Tuple (immutable, ordered)
coordinates = (10, 20)

# Dictionary (key-value pairs)
person = {"name": "John", "age": 30}
person["email"] = "john@example.com"

# Set (unique values)
unique_numbers = {1, 2, 3, 3, 4}  # {1, 2, 3, 4}
```

## Control Flow
```python
# If/elif/else
if age >= 18:
    print("Adult")
elif age >= 13:
    print("Teenager")
else:
    print("Child")

# For loop
for item in [1, 2, 3]:
    print(item)

# While loop
count = 0
while count < 5:
    print(count)
    count += 1

# List comprehension
squared = [x**2 for x in range(5)]
```

## Functions
```python
# Basic function
def greet(name):
    return f"Hello, {name}!"

# Default parameters
def power(base, exponent=2):
    return base ** exponent

# Lambda function
square = lambda x: x ** 2

# Decorators
@my_decorator
def my_function():
    pass
```

## Classes
```python
class Person:
    def __init__(self, name, age):
        self.name = name
        self.age = age

    def greet(self):
        return f"Hello, I'm {self.name}"

# Inheritance
class Employee(Person):
    def __init__(self, name, age, job_title):
        super().__init__(name, age)
        self.job_title = job_title
```

## File I/O
```python
# Read file
with open('file.txt', 'r') as f:
    content = f.read()

# Write file
with open('file.txt', 'w') as f:
    f.write('Hello, World!')

# Append to file
with open('file.txt', 'a') as f:
    f.write('New line\n')
```

## Common Operations
```python
# String manipulation
text.upper(), text.lower(), text.strip()
text.split(','), ','.join(list)
text.replace('old', 'new')

# List operations
len(list), sum(list), max(list), min(list)
list.append(item), list.pop(), list.remove(item)
list.sort(), sorted(list)

# Dictionary operations
dict.keys(), dict.values(), dict.items()
dict.get('key', default)

# Iteration
for index, value in enumerate(list):
    print(index, value)
```

## Error Handling
```python
try:
    risky_operation()
except ValueError as e:
    print(f"Error: {e}")
except Exception as e:
    print(f"Unexpected error: {e}")
finally:
    cleanup()
```

## Imports
```python
# Import module
import math
print(math.pi)

# Import specific functions
from datetime import datetime
now = datetime.now()

# Import with alias
import numpy as np
```

## Common Libraries
- **Standard Library**: os, sys, re, json, datetime, collections
- **Web**: requests, flask, django
- **Data**: pandas, numpy, matplotlib
- **Testing**: pytest, unittest

## Tips
- Use `snake_case` for variables and functions
- Use `PascalCase` for classes
- Use 4 spaces for indentation (not tabs)
- Use `if __name__ == '__main__':` for script entry point
- Use list comprehensions for concise transformations
- Use `with` statements for resource management
