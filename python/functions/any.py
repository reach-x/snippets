"""
any() - Python Built-in Function

Description:
Return True if bool(x) is True for any x in the iterable.

If the iterable is empty, return False.
"""

# Examples:

# Returns True if any element is true
result = any([False, False, True])
print(result)  # True

result = any([False, False, False])
print(result)  # False
