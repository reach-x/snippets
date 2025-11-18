"""
issubclass() - Python Built-in Function

Description:
Return whether 'cls' is derived from another class or is the same class.

A tuple, as in ``issubclass(x, (A, B, ...))``, may be given as the target to
check against. This is equivalent to ``issubclass(x, A) or issubclass(x, B)
or ...``.
"""

# Examples:

# Checks if class is subclass of another
result = issubclass(bool, int)
print(result)  # True

result = issubclass(int, bool)
print(result)  # False

class Parent:
    pass

class Child(Parent):
    pass

result = issubclass(Child, Parent)
print(result)  # True
