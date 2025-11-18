"""
map() - Python Built-in Function

Description:
Make an iterator that computes the function using arguments from
each of the iterables.  Stops when the shortest iterable is exhausted.
"""

# Examples:

# Applies function to all items
numbers = [1, 2, 3, 4]
squared = list(map(lambda x: x**2, numbers))
print(squared)  # [1, 4, 9, 16]

def double(x):
    return x * 2

result = list(map(double, [1, 2, 3]))
print(result)  # [2, 4, 6]
