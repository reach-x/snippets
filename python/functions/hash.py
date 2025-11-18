"""
hash() - Python Built-in Function

Description:
Return the hash value for the given object.

Two objects that compare equal must also have the same hash value, but the
reverse is not necessarily true.
"""

# Examples:

# Returns hash value of object
result = hash('hello')
print(result)

result = hash((1, 2, 3))
print(result)

# Lists are not hashable
# hash([1, 2, 3])  # TypeError
