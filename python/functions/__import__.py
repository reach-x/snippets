"""
__import__() - Python Built-in Function

Description:
Import a module.

Because this function is meant for use by the Python
interpreter and not for general use, it is better to use
importlib.import_module() to programmatically import a module.

The globals argument is only used to determine the context;
they are not modified.  The locals argument is unused.  The fromlist
should be a list of names to emulate ``from name import ...``, or an
empty list to emulate ``import name``.
When importing a module from a package, note that __import__('A.B', ...)
returns package A when fromlist is empty, but its submodule B when
fromlist is not empty.  The level argument is used to determine whether to
perform absolute or relative imports: 0 is absolute, while a positive number
is the number of parent directories to search relative to the current module.
"""

# Examples:

# Imports module dynamically
# math_module = __import__('math')
# print(math_module.pi)  # 3.141592653589793

# Generally prefer 'import' statement over __import__
pass
