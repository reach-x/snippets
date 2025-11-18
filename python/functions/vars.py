"""
vars() - Python Built-in Function

Description:
vars([object]) -> dictionary

Without arguments, equivalent to locals().
With an argument, equivalent to object.__dict__.
"""

# Examples:

# Returns __dict__ attribute
class MyClass:
    def __init__(self):
        self.x = 10
        self.y = 20

obj = MyClass()
result = vars(obj)
print(result)  # {'x': 10, 'y': 20}
