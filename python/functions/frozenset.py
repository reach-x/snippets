"""
frozenset() - Python Built-in Function

Description:
Build an immutable unordered collection of unique elements.
"""

# Examples:

# Creates immutable set
result = frozenset([1, 2, 3, 2, 1])
print(result)  # frozenset({1, 2, 3})

set1 = frozenset(['a', 'b', 'c'])
set2 = frozenset(['b', 'c', 'd'])
print(set1 & set2)  # frozenset({'b', 'c'})
