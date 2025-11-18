"""
all() - Python Built-in Function

Description:
Return True if bool(x) is True for all values x in the iterable.

If the iterable is empty, return True.
"""

# Examples:

# Returns True if all elements are true
result = all([True, True, True])
print(result)  # True

result = all([True, False, True])
print(result)  # False

result = all([1, 2, 3])
print(result)  # True
