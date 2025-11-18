"""
min() - Python Built-in Function

Description:
min(iterable, *[, default=obj, key=func]) -> value
min(arg1, arg2, *args, *[, key=func]) -> value

With a single iterable argument, return its smallest item. The
default keyword-only argument specifies an object to return if
the provided iterable is empty.
With two or more positional arguments, return the smallest argument.
"""

# Examples:

# Returns minimum value
result = min(1, 5, 3, 9, 2)
print(result)  # 1

result = min([10, 20, 30])
print(result)  # 10

result = min('apple', 'banana', 'cherry')
print(result)  # 'apple' 
