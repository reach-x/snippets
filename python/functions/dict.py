"""
dict() - Python Built-in Function

Description:
dict() -> new empty dictionary
dict(mapping) -> new dictionary initialized from a mapping object's
    (key, value) pairs
dict(iterable) -> new dictionary initialized as if via:
    d = {}
    for k, v in iterable:
        d[k] = v
dict(**kwargs) -> new dictionary initialized with the name=value pairs
    in the keyword argument list.  For example:  dict(one=1, two=2)
"""

# Examples:

# Creates dictionary
result = dict(a=1, b=2, c=3)
print(result)  # {'a': 1, 'b': 2, 'c': 3}

result = dict([('x', 100), ('y', 200)])
print(result)  # {'x': 100, 'y': 200}
