"""
staticmethod() - Python Built-in Function

Description:
Convert a function to be a static method.

A static method does not receive an implicit first argument.
To declare a static method, use this idiom:

     class C:
         @staticmethod
         def f(arg1, arg2, argN):
             ...

It can be called either on the class (e.g. C.f()) or on an instance
(e.g. C().f()). Both the class and the instance are ignored, and
neither is passed implicitly as the first argument to the method.

Static methods in Python are similar to those found in Java or C++.
For a more advanced concept, see the classmethod builtin.
"""

# Examples:

# Decorator for static methods
class MyClass:
    @staticmethod
    def static_method():
        return "This is a static method"

result = MyClass.static_method()
print(result)

obj = MyClass()
result = obj.static_method()
print(result)
