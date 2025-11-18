"""
range() - Python Built-in Function

Description:
range(stop) -> range object
range(start, stop[, step]) -> range object

Return an object that produces a sequence of integers from start (inclusive)
to stop (exclusive) by step.  range(i, j) produces i, i+1, i+2, ..., j-1.
start defaults to 0, and stop is omitted!  range(4) produces 0, 1, 2, 3.
These are exactly the valid indices for a list of 4 elements.
When step is given, it specifies the increment (or decrement).
"""

# Examples:

# Creates sequence of numbers
result = list(range(5))
print(result)  # [0, 1, 2, 3, 4]

result = list(range(2, 8))
print(result)  # [2, 3, 4, 5, 6, 7]

result = list(range(0, 10, 2))
print(result)  # [0, 2, 4, 6, 8]
