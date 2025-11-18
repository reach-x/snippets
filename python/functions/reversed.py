"""
reversed() - Python Built-in Function

Description:
Return a reverse iterator over the values of the given sequence.
"""

# Examples:

# Returns reversed iterator
result = list(reversed([1, 2, 3, 4]))
print(result)  # [4, 3, 2, 1]

result = list(reversed('hello'))
print(result)  # ['o', 'l', 'l', 'e', 'h']
