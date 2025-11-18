"""
list() - Python Built-in Function

Description:
Built-in mutable sequence.

If no argument is given, the constructor creates a new empty list.
The argument must be an iterable if specified.
"""

# Examples:

# Creates list
result = list((1, 2, 3))
print(result)  # [1, 2, 3]

result = list('hello')
print(result)  # ['h', 'e', 'l', 'l', 'o']

result = list(range(5))
print(result)  # [0, 1, 2, 3, 4]
