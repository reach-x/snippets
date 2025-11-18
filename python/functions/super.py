"""
super() - Python Built-in Function

Description:
super() -> same as super(__class__, <first argument>)
super(type) -> unbound super object
super(type, obj) -> bound super object; requires isinstance(obj, type)
super(type, type2) -> bound super object; requires issubclass(type2, type)
Typical use to call a cooperative superclass method:
class C(B):
    def meth(self, arg):
        super().meth(arg)
This works for class methods too:
class C(B):
    @classmethod
    def cmeth(cls, arg):
        super().cmeth(arg)
"""

# Examples:

# Returns proxy object for parent class
class Parent:
    def method(self):
        return "Parent method"

class Child(Parent):
    def method(self):
        parent_result = super().method()
        return f"Child method, {parent_result}"

child = Child()
print(child.method())
