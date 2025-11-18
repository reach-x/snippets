"""
object() - Python Built-in Function

Description:
The base class of the class hierarchy.

When called, it accepts no arguments and returns a new featureless
instance that has no instance attributes and cannot be given any.
"""

# Examples:

# Base class for all classes
obj = object()
print(type(obj))  # <class 'object'>

class MyClass(object):
    pass

instance = MyClass()
print(isinstance(instance, object))  # True
