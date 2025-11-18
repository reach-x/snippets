"""
exec() - Python Built-in Function

Description:
Execute the given source in the context of globals and locals.

The source may be a string representing one or more Python statements
or a code object as returned by compile().
The globals must be a dictionary and locals can be any mapping,
defaulting to the current globals and locals.
If only globals is given, locals defaults to it.
The closure must be a tuple of cellvars, and can only be used
when source is a code object requiring exactly that many cellvars.
"""

# Examples:

# Executes Python code dynamically
exec('x = 5')
exec('print(x)')  # 5

code = """
for i in range(3):
    print(i)
"""
exec(code)
