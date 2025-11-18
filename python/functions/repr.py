"""
repr() - Python Built-in Function

Description:
Return the canonical string representation of the object.

For many object types, including most builtins, eval(repr(obj)) == obj.
"""

# Examples:

# Returns printable representation
result = repr('hello')
print(result)  # "'hello'"

result = repr([1, 2, 3])
print(result)  # '[1, 2, 3]' 
