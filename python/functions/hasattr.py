"""
hasattr() - Python Built-in Function

Description:
Return whether the object has an attribute with the given name.

This is done by calling getattr(obj, name) and catching AttributeError.
"""

# Examples:

# Checks if object has attribute
class MyClass:
    x = 10

obj = MyClass()
result = hasattr(obj, 'x')
print(result)  # True

result = hasattr(obj, 'y')
print(result)  # False
