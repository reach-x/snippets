"""
globals() - Python Built-in Function

Description:
Return the dictionary containing the current scope's global variables.

NOTE: Updates to this dictionary *will* affect name lookups in the current
global scope and vice-versa.
"""

# Examples:

# Returns dictionary of global symbol table
x = 10
result = globals()
print('x' in result)  # True
print(result['x'])  # 10
