"""
setattr() - Python Built-in Function

Description:
Sets the named attribute on the given object to the specified value.

setattr(x, 'y', v) is equivalent to ``x.y = v``
"""

# Examples:

# Sets attribute on object
class MyClass:
    pass

obj = MyClass()
setattr(obj, 'x', 10)
print(obj.x)  # 10

setattr(obj, 'y', 20)
print(obj.y)  # 20
