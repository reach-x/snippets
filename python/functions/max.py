"""
max() - Python Built-in Function

Description:
max(iterable, *[, default=obj, key=func]) -> value
max(arg1, arg2, *args, *[, key=func]) -> value

With a single iterable argument, return its biggest item. The
default keyword-only argument specifies an object to return if
the provided iterable is empty.
With two or more positional arguments, return the largest argument.
"""

# Examples:

# Returns maximum value
result = max(1, 5, 3, 9, 2)
print(result)  # 9

result = max([10, 20, 30])
print(result)  # 30

result = max('apple', 'banana', 'cherry')
print(result)  # 'cherry' 
