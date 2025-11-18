"""
callable() - Python Built-in Function

Description:
Return whether the object is callable (i.e., some kind of function).

Note that classes are callable, as are instances of classes with a
__call__() method.
"""

# Examples:

# Checks if object is callable
def my_function():
    pass

result = callable(my_function)
print(result)  # True

result = callable(42)
print(result)  # False
