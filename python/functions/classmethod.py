"""
classmethod() - Python Built-in Function

Description:
Convert a function to be a class method.

A class method receives the class as implicit first argument,
just like an instance method receives the instance.
To declare a class method, use this idiom:

  class C:
      @classmethod
      def f(cls, arg1, arg2, argN):
          ...

It can be called either on the class (e.g. C.f()) or on an instance
(e.g. C().f()).  The instance is ignored except for its class.
If a class method is called for a derived class, the derived class
object is passed as the implied first argument.

Class methods are different than C++ or Java static methods.
If you want those, see the staticmethod builtin.
"""

# Examples:

# Decorator for class methods
class MyClass:
    value = 10

    @classmethod
    def class_method(cls):
        return cls.value

result = MyClass.class_method()
print(result)  # 10
