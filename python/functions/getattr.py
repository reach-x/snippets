"""
getattr() - Python Built-in Function

Description:
getattr(object, name[, default]) -> value

Get a named attribute from an object; getattr(x, 'y') is equivalent to x.y.
When a default argument is given, it is returned when the attribute doesn't
exist; without it, an exception is raised in that case.
"""

# Examples:

# Gets attribute value from object
class MyClass:
    x = 10
    y = 20

obj = MyClass()
result = getattr(obj, 'x')
print(result)  # 10

result = getattr(obj, 'z', 'default')
print(result)  # 'default' 
