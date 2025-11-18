"""
sum() - Python Built-in Function

Description:
Return the sum of a 'start' value (default: 0) plus an iterable of numbers

When the iterable is empty, return the start value.
This function is intended specifically for use with numeric values and may
reject non-numeric types.
"""

# Examples:

# Sums items of iterable
result = sum([1, 2, 3, 4])
print(result)  # 10

result = sum([1.5, 2.5, 3.0])
print(result)  # 7.0

result = sum([1, 2, 3], 10)  # Start value
print(result)  # 16
