"""
Python Interview Questions: Common Patterns and Best Practices
===============================================================
Topics: Pythonic code, idioms, best practices, common patterns
Greppable format for quick reference
"""

# ============================================================================
# PYTHONIC CODE (PEP 8 and Idioms)
# ============================================================================

# Q: What is PEP 8?
# A: Python's official style guide for writing clean, readable code

# Q: What are common PEP 8 naming conventions?
# A:
# - Variables/functions: snake_case
# - Classes: PascalCase
# - Constants: UPPER_CASE_WITH_UNDERSCORES
# - Private: _leading_underscore
# - Protected: __double_leading_underscore

# Q: How do you swap variables pythonically?
# A: Use tuple unpacking
a, b = 10, 20
a, b = b, a  # Pythonic swap

# Q: How do you check if variable is None?
# A: Use 'is None' not '== None'
value = None
if value is None:  # Good
    pass
# if value == None:  # Bad

# Q: How do you check if list/string is empty?
# A: Use truthiness - empty containers are falsy
my_list = []
if not my_list:  # Good
    print("Empty")
# if len(my_list) == 0:  # Bad

# Q: What is the EAFP principle?
# A: "Easier to Ask for Forgiveness than Permission" - use try/except
# Good (EAFP)
try:
    value = my_dict[key]
except KeyError:
    value = default_value

# Bad (LBYL - Look Before You Leap)
if key in my_dict:
    value = my_dict[key]
else:
    value = default_value

# Q: How do you open files pythonically?
# A: Use context manager (with statement)
# Good
with open('file.txt', 'r') as f:
    content = f.read()

# Bad
f = open('file.txt', 'r')
content = f.read()
f.close()

# Q: How do you chain comparisons?
# A: Python allows natural mathematical chaining
x = 5
if 1 < x < 10:  # Pythonic
    pass
# if x > 1 and x < 10:  # Works but not pythonic

# Q: How do you use enumerate instead of range(len())?
# A: enumerate() gives index and value
items = ['a', 'b', 'c']
# Good
for i, item in enumerate(items):
    print(f"{i}: {item}")
# Bad
for i in range(len(items)):
    print(f"{i}: {items[i]}")

# Q: How do you use zip for parallel iteration?
# A: zip() combines multiple iterables
names = ['Alice', 'Bob']
ages = [25, 30]
# Good
for name, age in zip(names, ages):
    print(f"{name} is {age}")
# Bad
for i in range(len(names)):
    print(f"{names[i]} is {ages[i]}")

# ============================================================================
# STRING FORMATTING
# ============================================================================

# Q: What are the different ways to format strings?
# A: f-strings (preferred), format(), % operator
name = "Alice"
age = 30

# f-strings (Python 3.6+) - BEST
message = f"{name} is {age} years old"

# format() method
message = "{} is {} years old".format(name, age)
message = "{name} is {age} years old".format(name=name, age=age)

# % operator (old style)
message = "%s is %d years old" % (name, age)

# Q: How do you format numbers in f-strings?
# A: Use format specifiers
pi = 3.14159
print(f"{pi:.2f}")  # 3.14
print(f"{pi:.4f}")  # 3.1416

number = 1234567
print(f"{number:,}")  # 1,234,567
print(f"{number:_}")  # 1_234_567

percentage = 0.847
print(f"{percentage:.1%}")  # 84.7%

# ============================================================================
# LIST/DICT MANIPULATION PATTERNS
# ============================================================================

# Q: How do you remove duplicates from list while preserving order?
# A: Use dict.fromkeys() or set (if order doesn't matter)
items = [1, 2, 3, 2, 1, 4]
# Preserve order
unique = list(dict.fromkeys(items))
# Don't care about order
unique = list(set(items))

# Q: How do you merge dictionaries?
# A: Use ** operator or | operator (Python 3.9+)
dict1 = {'a': 1, 'b': 2}
dict2 = {'c': 3, 'd': 4}

# Python 3.5+
merged = {**dict1, **dict2}

# Python 3.9+
merged = dict1 | dict2

# Old way
merged = dict1.copy()
merged.update(dict2)

# Q: How do you reverse a dictionary (swap keys and values)?
# A: Use dictionary comprehension
original = {'a': 1, 'b': 2, 'c': 3}
reversed_dict = {v: k for k, v in original.items()}

# Q: How do you sort dictionary by value?
# A: Use sorted() with key parameter
scores = {'Alice': 85, 'Bob': 92, 'Charlie': 78}
sorted_by_value = dict(sorted(scores.items(), key=lambda item: item[1]))
sorted_desc = dict(sorted(scores.items(), key=lambda item: item[1], reverse=True))

# Q: How do you group items by property?
# A: Use defaultdict or itertools.groupby
from collections import defaultdict

people = [
    {'name': 'Alice', 'city': 'NYC'},
    {'name': 'Bob', 'city': 'LA'},
    {'name': 'Charlie', 'city': 'NYC'}
]

by_city = defaultdict(list)
for person in people:
    by_city[person['city']].append(person['name'])

# Q: How do you flatten a list of lists?
# A: Use list comprehension or itertools.chain
nested = [[1, 2], [3, 4], [5, 6]]

# List comprehension
flattened = [item for sublist in nested for item in sublist]

# itertools.chain
from itertools import chain
flattened = list(chain.from_iterable(nested))

# Q: How do you get unique elements in order of first appearance?
# A: Use dict.fromkeys()
items = [3, 1, 4, 1, 5, 9, 2, 6, 5]
unique = list(dict.fromkeys(items))

# ============================================================================
# DEFAULT VALUES AND MISSING DATA
# ============================================================================

# Q: How do you provide default values for dictionary keys?
# A: Use get() method or defaultdict
my_dict = {'a': 1, 'b': 2}

# get() method
value = my_dict.get('c', 0)  # Returns 0 if 'c' doesn't exist

# setdefault()
value = my_dict.setdefault('c', 0)  # Sets and returns if doesn't exist

# defaultdict
from collections import defaultdict
counts = defaultdict(int)
counts['a'] += 1  # No KeyError, defaults to 0

# Q: How do you use Counter for counting?
# A: collections.Counter is a dict subclass for counting
from collections import Counter

words = ['apple', 'banana', 'apple', 'cherry', 'banana', 'apple']
word_counts = Counter(words)
print(word_counts.most_common(2))  # [('apple', 3), ('banana', 2)]

# Q: How do you use namedtuple?
# A: Create lightweight objects with named fields
from collections import namedtuple

Point = namedtuple('Point', ['x', 'y'])
p = Point(10, 20)
print(p.x, p.y)  # 10 20

# More readable than tuple
# p[0], p[1] vs p.x, p.y

# ============================================================================
# FILE I/O PATTERNS
# ============================================================================

# Q: How do you read entire file?
# A: Use read() method with context manager
with open('file.txt', 'r') as f:
    content = f.read()

# Q: How do you read file line by line?
# A: Iterate over file object directly
with open('file.txt', 'r') as f:
    for line in f:
        print(line.strip())

# Q: How do you read all lines into list?
# A: Use readlines()
with open('file.txt', 'r') as f:
    lines = f.readlines()

# Q: How do you write to file?
# A: Use write() or writelines()
with open('file.txt', 'w') as f:
    f.write('Hello\n')
    f.writelines(['Line 1\n', 'Line 2\n'])

# Q: How do you append to file?
# A: Use 'a' mode
with open('file.txt', 'a') as f:
    f.write('Appended line\n')

# Q: How do you work with JSON files?
# A: Use json module
import json

# Write JSON
data = {'name': 'Alice', 'age': 30}
with open('data.json', 'w') as f:
    json.dump(data, f, indent=2)

# Read JSON
with open('data.json', 'r') as f:
    data = json.load(f)

# Q: How do you work with CSV files?
# A: Use csv module
import csv

# Write CSV
with open('data.csv', 'w', newline='') as f:
    writer = csv.writer(f)
    writer.writerow(['Name', 'Age'])
    writer.writerow(['Alice', 30])

# Read CSV
with open('data.csv', 'r') as f:
    reader = csv.reader(f)
    for row in reader:
        print(row)

# ============================================================================
# ERROR HANDLING PATTERNS
# ============================================================================

# Q: How do you handle multiple exception types?
# A: Use tuple in except clause
try:
    result = risky_operation()
except (ValueError, TypeError) as e:
    print(f"Error: {e}")

# Q: How do you re-raise exceptions?
# A: Use raise without arguments
try:
    risky_operation()
except Exception as e:
    print(f"Logging: {e}")
    raise  # Re-raises the same exception

# Q: How do you suppress exceptions?
# A: Use contextlib.suppress
from contextlib import suppress

with suppress(FileNotFoundError):
    with open('nonexistent.txt') as f:
        content = f.read()

# Q: What is try-except-else-finally?
# A: else runs if no exception, finally always runs
try:
    result = operation()
except Exception as e:
    print(f"Error: {e}")
else:
    print("Success")
finally:
    cleanup()

# ============================================================================
# PERFORMANCE PATTERNS
# ============================================================================

# Q: How do you measure execution time?
# A: Use time.perf_counter() or timeit module
import time

start = time.perf_counter()
# Code to measure
end = time.perf_counter()
print(f"Time: {end - start}")

# For benchmarking
import timeit
time_taken = timeit.timeit('sum(range(100))', number=10000)

# Q: When to use list vs generator?
# A: Use generator for large datasets or one-time iteration
# List - when need multiple iterations or indexing
numbers_list = [x**2 for x in range(1000000)]

# Generator - when iterate once, memory efficient
numbers_gen = (x**2 for x in range(1000000))

# Q: How do you use sets for faster membership testing?
# A: Sets use hash tables - O(1) lookup vs O(n) for lists
# Slow
items_list = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
if 5 in items_list:  # O(n)
    pass

# Fast
items_set = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10}
if 5 in items_set:  # O(1)
    pass

# Q: How do you avoid repeated function calls?
# A: Cache results using variables or @lru_cache
# Bad
for i in range(len(my_list)):  # len() called multiple times
    pass

# Good
length = len(my_list)
for i in range(length):
    pass

# ============================================================================
# FUNCTIONAL PROGRAMMING PATTERNS
# ============================================================================

# Q: How do you use map, filter, reduce?
# A: Apply functions to iterables
from functools import reduce

numbers = [1, 2, 3, 4, 5]

# map - transform each element
squared = list(map(lambda x: x**2, numbers))

# filter - select elements
evens = list(filter(lambda x: x % 2 == 0, numbers))

# reduce - accumulate to single value
product = reduce(lambda x, y: x * y, numbers)

# Q: When to use list comprehension vs map/filter?
# A: List comprehension is usually more pythonic and readable
# List comprehension (preferred)
squared = [x**2 for x in numbers]
evens = [x for x in numbers if x % 2 == 0]

# map/filter (when passing existing function)
squared = list(map(square_function, numbers))

# ============================================================================
# COMMON ANTI-PATTERNS (WHAT TO AVOID)
# ============================================================================

# Q: What are common Python anti-patterns?
# A: Here are patterns to avoid:

# 1. Using mutable default arguments
# BAD
def append_to_list(item, my_list=[]):
    my_list.append(item)
    return my_list

# GOOD
def append_to_list(item, my_list=None):
    if my_list is None:
        my_list = []
    my_list.append(item)
    return my_list

# 2. Comparing boolean with == True
# BAD
if my_bool == True:
    pass

# GOOD
if my_bool:
    pass

# 3. Using bare except
# BAD
try:
    operation()
except:  # Catches everything, even KeyboardInterrupt
    pass

# GOOD
try:
    operation()
except Exception as e:  # Specific exceptions
    handle_error(e)

# 4. Not using 'with' for file operations
# BAD
f = open('file.txt')
content = f.read()
f.close()  # Might not execute if exception occurs

# GOOD
with open('file.txt') as f:
    content = f.read()

# 5. Using + for string concatenation in loops
# BAD
result = ""
for s in strings:
    result += s  # Creates new string each iteration

# GOOD
result = "".join(strings)

# 6. Not using items() when iterating dict
# BAD
for key in my_dict:
    value = my_dict[key]

# GOOD
for key, value in my_dict.items():
    pass

# ============================================================================
# PYTHONIC TRICKS AND TIPS
# ============================================================================

# Q: How do you use walrus operator (Python 3.8+)?
# A: := assigns and returns value in same expression
# Useful in while loops and if statements
numbers = [1, 2, 3, 4, 5]
if (length := len(numbers)) > 3:
    print(f"Length is {length}")

# Read file until empty line
# while (line := input()) != "":
#     print(line)

# Q: How do you use unpacking effectively?
# A: Unpack sequences into variables
# Basic unpacking
x, y, z = [1, 2, 3]

# Extended unpacking with *
first, *middle, last = [1, 2, 3, 4, 5]
# first=1, middle=[2, 3, 4], last=5

# Dictionary unpacking
def greet(name, age):
    return f"{name} is {age}"

person = {'name': 'Alice', 'age': 30}
message = greet(**person)

# Q: How do you use else with loops?
# A: else clause executes if loop completes without break
for i in range(5):
    if i == 10:
        break
else:
    print("Loop completed normally")

# Q: How do you use ternary operator?
# A: value_if_true if condition else value_if_false
age = 25
status = "adult" if age >= 18 else "minor"

# Q: How do you unpack in function definitions?
# A: Use * for positional, ** for keyword arguments
def function(a, b, *args, **kwargs):
    print(f"a={a}, b={b}")
    print(f"args={args}")
    print(f"kwargs={kwargs}")

function(1, 2, 3, 4, x=5, y=6)

# Q: How do you use multiple context managers?
# A: Separate with commas or use contextlib.ExitStack
# Python 3.1+
with open('in.txt') as fin, open('out.txt', 'w') as fout:
    fout.write(fin.read())
