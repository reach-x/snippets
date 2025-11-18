"""
eval() - Python Built-in Function

Description:
Evaluate the given source in the context of globals and locals.

The source may be a string representing a Python expression
or a code object as returned by compile().
The globals must be a dictionary and locals can be any mapping,
defaulting to the current globals and locals.
If only globals is given, locals defaults to it.
"""

# Examples:

# Evaluates string expression
result = eval('2 + 3')
print(result)  # 5

x = 10
result = eval('x * 2')
print(result)  # 20
