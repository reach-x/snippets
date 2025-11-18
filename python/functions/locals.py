"""
locals() - Python Built-in Function

Description:
Return a dictionary containing the current scope's local variables.

NOTE: Whether or not updates to this dictionary will affect name lookups in
the local scope and vice-versa is *implementation dependent* and not
covered by any backwards compatibility guarantees.
"""

# Examples:

# Returns dictionary of local symbol table
def my_function():
    x = 10
    y = 20
    result = locals()
    print(result)  # {'x': 10, 'y': 20}

my_function()
