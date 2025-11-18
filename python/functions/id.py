"""
id() - Python Built-in Function

Description:
Return the identity of an object.

This is guaranteed to be unique among simultaneously existing objects.
(CPython uses the object's memory address.)
"""

# Examples:

# Returns identity of object
x = [1, 2, 3]
result = id(x)
print(result)  # Unique identifier

y = x
print(id(y) == id(x))  # True (same object)
