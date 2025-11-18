"""
sorted() - Python Built-in Function

Description:
Return a new list containing all items from the iterable in ascending order.

A custom key function can be supplied to customize the sort order, and the
reverse flag can be set to request the result in descending order.
"""

# Examples:

# Returns sorted list
result = sorted([3, 1, 4, 1, 5])
print(result)  # [1, 1, 3, 4, 5]

result = sorted(['banana', 'apple', 'cherry'])
print(result)  # ['apple', 'banana', 'cherry']

result = sorted([3, 1, 4], reverse=True)
print(result)  # [4, 3, 1]
