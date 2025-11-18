"""
type() - Python Built-in Function

Description:
type(object) -> the object's type
type(name, bases, dict, **kwds) -> a new type
"""

# Examples:

# Returns type of object
result = type(42)
print(result)  # <class 'int'>

result = type('hello')
print(result)  # <class 'str'>

result = type([1, 2, 3])
print(result)  # <class 'list'>
