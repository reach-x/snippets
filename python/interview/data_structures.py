"""
Python Interview Questions: Data Structures
============================================
Topics: Lists, Tuples, Dictionaries, Sets, Strings
Greppable format for quick reference
"""

# ============================================================================
# LISTS - Mutable sequences
# ============================================================================

# Q: What is a list in Python?
# A: A list is a mutable, ordered collection that can hold items of different types.
# Example:
my_list = [1, 2, 3, "four", 5.0]

# Q: How do you create an empty list?
# A: Using [] or list()
empty_list_1 = []
empty_list_2 = list()

# Q: How do you append an item to a list?
# A: Use the append() method
numbers = [1, 2, 3]
numbers.append(4)  # [1, 2, 3, 4]

# Q: How do you extend a list with another list?
# A: Use extend() method or + operator
list_a = [1, 2, 3]
list_b = [4, 5, 6]
list_a.extend(list_b)  # [1, 2, 3, 4, 5, 6]
# OR
list_c = list_a + list_b

# Q: How do you insert an item at a specific position?
# A: Use insert(index, item)
numbers = [1, 2, 4]
numbers.insert(2, 3)  # [1, 2, 3, 4]

# Q: How do you remove an item from a list?
# A: Use remove(value), pop(index), or del
numbers = [1, 2, 3, 4, 5]
numbers.remove(3)  # Removes first occurrence of 3
popped = numbers.pop()  # Removes and returns last item
popped_at_index = numbers.pop(0)  # Removes and returns item at index 0
del numbers[0]  # Deletes item at index 0

# Q: How do you reverse a list?
# A: Use reverse() method or reversed() function
numbers = [1, 2, 3, 4, 5]
numbers.reverse()  # In-place reversal
# OR
reversed_list = list(reversed(numbers))  # Returns new list

# Q: How do you sort a list?
# A: Use sort() method or sorted() function
numbers = [3, 1, 4, 1, 5, 9, 2, 6]
numbers.sort()  # In-place sort
# OR
sorted_list = sorted(numbers)  # Returns new sorted list
sorted_desc = sorted(numbers, reverse=True)  # Descending order

# Q: How do you find the index of an item?
# A: Use index() method
fruits = ["apple", "banana", "cherry"]
banana_index = fruits.index("banana")  # Returns 1

# Q: How do you count occurrences of an item?
# A: Use count() method
numbers = [1, 2, 3, 2, 4, 2, 5]
count_of_twos = numbers.count(2)  # Returns 3

# Q: How do you slice a list?
# A: Use list[start:stop:step] notation
numbers = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
first_five = numbers[:5]  # [0, 1, 2, 3, 4]
last_five = numbers[-5:]  # [5, 6, 7, 8, 9]
middle = numbers[3:7]  # [3, 4, 5, 6]
every_other = numbers[::2]  # [0, 2, 4, 6, 8]
reversed_slice = numbers[::-1]  # [9, 8, 7, 6, 5, 4, 3, 2, 1, 0]

# Q: How do you copy a list?
# A: Use copy() method, list() constructor, or slicing
original = [1, 2, 3]
copy_1 = original.copy()
copy_2 = list(original)
copy_3 = original[:]
# Note: These are shallow copies

# Q: What is list comprehension?
# A: A concise way to create lists based on existing iterables
squares = [x**2 for x in range(10)]
even_squares = [x**2 for x in range(10) if x % 2 == 0]

# Q: How do you flatten a nested list?
# A: Use nested list comprehension or itertools.chain
nested = [[1, 2], [3, 4], [5, 6]]
flattened = [item for sublist in nested for item in sublist]
# OR
from itertools import chain
flattened = list(chain.from_iterable(nested))

# ============================================================================
# TUPLES - Immutable sequences
# ============================================================================

# Q: What is a tuple and how is it different from a list?
# A: A tuple is an immutable, ordered collection. Once created, it cannot be modified.
my_tuple = (1, 2, 3, "four")

# Q: How do you create a single-element tuple?
# A: Use a trailing comma
single_tuple = (42,)  # Note the comma
not_a_tuple = (42)  # This is just an integer in parentheses

# Q: Can you modify a tuple?
# A: No, tuples are immutable. You must create a new tuple.
# original = (1, 2, 3)
# original[0] = 10  # This raises TypeError

# Q: What are the advantages of tuples over lists?
# A: 1) Faster than lists, 2) Can be used as dict keys, 3) Protect data from modification
coordinates = (40.7128, -74.0060)  # Lat/Long shouldn't change

# Q: How do you unpack a tuple?
# A: Assign to multiple variables
point = (10, 20, 30)
x, y, z = point

# Q: How do you use tuple unpacking with functions?
# A: Functions can return multiple values as a tuple
def get_min_max(numbers):
    return min(numbers), max(numbers)

minimum, maximum = get_min_max([1, 2, 3, 4, 5])

# Q: What is the * operator for tuple unpacking?
# A: It captures multiple values
first, *middle, last = [1, 2, 3, 4, 5]
# first = 1, middle = [2, 3, 4], last = 5

# ============================================================================
# DICTIONARIES - Key-value mappings
# ============================================================================

# Q: What is a dictionary?
# A: A dictionary is a mutable, unordered collection of key-value pairs.
person = {"name": "John", "age": 30, "city": "New York"}

# Q: How do you create an empty dictionary?
# A: Use {} or dict()
empty_dict_1 = {}
empty_dict_2 = dict()

# Q: How do you access a value in a dictionary?
# A: Use dict[key] or dict.get(key)
name = person["name"]  # Raises KeyError if key doesn't exist
age = person.get("age")  # Returns None if key doesn't exist
age_or_default = person.get("height", 0)  # Returns 0 if key doesn't exist

# Q: How do you add or update a key-value pair?
# A: Use dict[key] = value
person["email"] = "john@example.com"  # Add new key
person["age"] = 31  # Update existing key

# Q: How do you remove a key-value pair?
# A: Use del, pop(), or popitem()
del person["city"]  # Remove specific key
email = person.pop("email")  # Remove and return value
last_item = person.popitem()  # Remove and return last inserted pair

# Q: How do you check if a key exists?
# A: Use the 'in' operator
if "name" in person:
    print(person["name"])

# Q: How do you iterate over a dictionary?
# A: Use items(), keys(), or values()
for key in person.keys():
    print(key)
for value in person.values():
    print(value)
for key, value in person.items():
    print(f"{key}: {value}")

# Q: How do you merge two dictionaries?
# A: Use update() or ** operator (Python 3.5+) or | operator (Python 3.9+)
dict1 = {"a": 1, "b": 2}
dict2 = {"c": 3, "d": 4}
dict1.update(dict2)  # Modifies dict1
# OR
merged = {**dict1, **dict2}  # Creates new dict
# OR (Python 3.9+)
merged = dict1 | dict2

# Q: What is dictionary comprehension?
# A: A concise way to create dictionaries
squares_dict = {x: x**2 for x in range(5)}
# {0: 0, 1: 1, 2: 4, 3: 9, 4: 16}

# Q: How do you get all keys or values as a list?
# A: Use list(dict.keys()) or list(dict.values())
keys_list = list(person.keys())
values_list = list(person.values())

# Q: What is setdefault() used for?
# A: Returns value if key exists, otherwise sets and returns default value
person.setdefault("country", "USA")  # Adds "country": "USA" if not exists

# Q: How do you create a default dictionary?
# A: Use collections.defaultdict
from collections import defaultdict
word_count = defaultdict(int)
word_count["apple"] += 1  # No KeyError, defaults to 0

# Q: How do you sort a dictionary?
# A: Dictionaries are unordered, but you can get sorted items
sorted_by_key = dict(sorted(person.items()))
sorted_by_value = dict(sorted(person.items(), key=lambda item: item[1]))

# ============================================================================
# SETS - Unordered unique collections
# ============================================================================

# Q: What is a set?
# A: A set is an unordered collection of unique elements.
my_set = {1, 2, 3, 4, 5}

# Q: How do you create an empty set?
# A: Use set() - NOT {} which creates an empty dict
empty_set = set()

# Q: How do you add an item to a set?
# A: Use add() method
numbers = {1, 2, 3}
numbers.add(4)  # {1, 2, 3, 4}
numbers.add(2)  # Still {1, 2, 3, 4} - duplicates ignored

# Q: How do you remove an item from a set?
# A: Use remove(), discard(), or pop()
numbers.remove(4)  # Raises KeyError if not found
numbers.discard(10)  # Does not raise error if not found
random_item = numbers.pop()  # Removes and returns arbitrary item

# Q: What are set operations?
# A: Union, intersection, difference, symmetric difference
set_a = {1, 2, 3, 4}
set_b = {3, 4, 5, 6}

union = set_a | set_b  # {1, 2, 3, 4, 5, 6}
intersection = set_a & set_b  # {3, 4}
difference = set_a - set_b  # {1, 2} - in a but not in b
symmetric_diff = set_a ^ set_b  # {1, 2, 5, 6} - in either but not both

# Q: How do you check subset and superset?
# A: Use issubset() and issuperset() or <= and >=
set_c = {1, 2}
is_subset = set_c.issubset(set_a)  # True
is_superset = set_a.issuperset(set_c)  # True
# OR
is_subset = set_c <= set_a
is_superset = set_a >= set_c

# Q: How do you convert a list to a set to remove duplicates?
# A: Use set() constructor
numbers_with_dupes = [1, 2, 2, 3, 3, 3, 4]
unique_numbers = list(set(numbers_with_dupes))

# Q: What is a frozenset?
# A: An immutable version of a set that can be used as a dictionary key
frozen = frozenset([1, 2, 3])
# frozen.add(4)  # This raises AttributeError

# ============================================================================
# STRINGS - Immutable sequences of characters
# ============================================================================

# Q: How do you concatenate strings?
# A: Use + operator or join() method
full_name = "John" + " " + "Doe"
words = ["Hello", "World"]
sentence = " ".join(words)  # "Hello World"

# Q: How do you format strings?
# A: Use f-strings (Python 3.6+), format(), or % operator
name = "Alice"
age = 30
# f-strings (preferred)
message = f"My name is {name} and I am {age} years old"
# format method
message = "My name is {} and I am {} years old".format(name, age)
# % operator (old style)
message = "My name is %s and I am %d years old" % (name, age)

# Q: How do you check if a substring exists?
# A: Use 'in' operator or find() method
text = "Hello World"
contains_hello = "Hello" in text  # True
position = text.find("World")  # Returns 6, or -1 if not found

# Q: How do you split a string?
# A: Use split() method
sentence = "Python is awesome"
words = sentence.split()  # ["Python", "is", "awesome"]
csv_data = "apple,banana,cherry"
fruits = csv_data.split(",")  # ["apple", "banana", "cherry"]

# Q: How do you replace text in a string?
# A: Use replace() method
text = "Hello World"
new_text = text.replace("World", "Python")  # "Hello Python"

# Q: How do you convert string case?
# A: Use upper(), lower(), capitalize(), title()
text = "hello world"
upper_text = text.upper()  # "HELLO WORLD"
lower_text = text.lower()  # "hello world"
capitalized = text.capitalize()  # "Hello world"
title_case = text.title()  # "Hello World"

# Q: How do you remove whitespace?
# A: Use strip(), lstrip(), rstrip()
text = "  hello world  "
trimmed = text.strip()  # "hello world"
left_trimmed = text.lstrip()  # "hello world  "
right_trimmed = text.rstrip()  # "  hello world"

# Q: How do you check string properties?
# A: Use isalpha(), isdigit(), isalnum(), etc.
text = "Python3"
is_alpha = text.isalpha()  # False
is_digit = "123".isdigit()  # True
is_alnum = text.isalnum()  # True
is_upper = "HELLO".isupper()  # True
is_lower = "hello".islower()  # True

# Q: How do you count occurrences of a substring?
# A: Use count() method
text = "hello hello world"
count = text.count("hello")  # Returns 2

# Q: How do you reverse a string?
# A: Use slicing
text = "Python"
reversed_text = text[::-1]  # "nohtyP"

# Q: How do you check if string starts or ends with something?
# A: Use startswith() and endswith()
filename = "document.pdf"
is_pdf = filename.endswith(".pdf")  # True
starts_with_doc = filename.startswith("doc")  # True

# Q: Are strings mutable or immutable?
# A: Strings are IMMUTABLE. Any operation that modifies a string creates a new string.
text = "hello"
# text[0] = "H"  # This raises TypeError
new_text = "H" + text[1:]  # This creates a new string "Hello"
