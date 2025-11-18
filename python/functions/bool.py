"""
bool() - Python Built-in Function

Description:
Returns True when the argument is true, False otherwise.
The builtins True and False are the only two instances of the class bool.
The class bool is a subclass of the class int, and cannot be subclassed.
"""

# Examples:

# Converts value to Boolean
result = bool(1)
print(result)  # True

result = bool(0)
print(result)  # False

result = bool([])
print(result)  # False

result = bool([1, 2])
print(result)  # True
