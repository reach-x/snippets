"""
slice() - Python Built-in Function

Description:
slice(stop)
slice(start, stop[, step])

Create a slice object.  This is used for extended slicing (e.g. a[0:10:2]).
"""

# Examples:

# Creates slice object
my_list = [0, 1, 2, 3, 4, 5]
s = slice(1, 5, 2)
result = my_list[s]
print(result)  # [1, 3]

s = slice(None, None, -1)
result = my_list[s]
print(result)  # [5, 4, 3, 2, 1, 0]
