"""
tuple() - Python Built-in Function

Description:
Built-in immutable sequence.

If no argument is given, the constructor returns an empty tuple.
If iterable is specified the tuple is initialized from iterable's items.

If the argument is a tuple, the return value is the same object.
"""

# Examples:

# Creates tuple
result = tuple([1, 2, 3])
print(result)  # (1, 2, 3)

result = tuple('hello')
print(result)  # ('h', 'e', 'l', 'l', 'o')

result = tuple()
print(result)  # ()
