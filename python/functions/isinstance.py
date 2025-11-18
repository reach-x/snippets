"""
isinstance() - Python Built-in Function

Description:
Return whether an object is an instance of a class or of a subclass thereof.

A tuple, as in ``isinstance(x, (A, B, ...))``, may be given as the target to
check against. This is equivalent to ``isinstance(x, A) or isinstance(x, B)
or ...`` etc.
"""

# Examples:

# Checks if object is instance of class
result = isinstance(5, int)
print(result)  # True

result = isinstance('hello', str)
print(result)  # True

result = isinstance([1, 2], (list, tuple))
print(result)  # True
