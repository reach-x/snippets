"""
filter() - Python Built-in Function

Description:
Return an iterator yielding those items of iterable for which function(item)
is true. If function is None, return the items that are true.
"""

# Examples:

# Filters sequence based on function
numbers = [1, 2, 3, 4, 5, 6]
even_numbers = list(filter(lambda x: x % 2 == 0, numbers))
print(even_numbers)  # [2, 4, 6]

def is_positive(n):
    return n > 0

result = list(filter(is_positive, [-2, -1, 0, 1, 2]))
print(result)  # [1, 2]
