"""
delattr() - Python Built-in Function

Description:
Deletes the named attribute from the given object.

delattr(x, 'y') is equivalent to ``del x.y``
"""

# Examples:

# Deletes attribute from object
class MyClass:
    attr = 10

obj = MyClass()
delattr(obj, 'attr')
# print(obj.attr)  # Would raise AttributeError
