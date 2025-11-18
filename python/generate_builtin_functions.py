"""
Generate individual files for all Python built-in functions.
Each file will contain documentation and examples for that function.
"""

import os
import inspect

# Get all built-in functions
builtins_list = dir(__builtins__)

# Filter to get only functions (exclude exceptions, constants, etc.)
builtin_functions = []
for name in builtins_list:
    obj = getattr(__builtins__, name)
    # Include functions and types/classes that are commonly used as functions
    if callable(obj) or name in ['bool', 'int', 'float', 'str', 'list', 'tuple', 'dict', 'set', 'frozenset', 'bytes', 'bytearray']:
        # Exclude private/dunder names except __import__
        if not name.startswith('_') or name == '__import__':
            builtin_functions.append(name)

# Sort for consistent output
builtin_functions.sort()

print(f"Found {len(builtin_functions)} built-in functions:")
print(builtin_functions)

# Create functions directory if it doesn't exist
functions_dir = "functions"
os.makedirs(functions_dir, exist_ok=True)

# Template for each function file
def get_function_content(func_name):
    obj = getattr(__builtins__, func_name)

    # Get docstring
    doc = inspect.getdoc(obj) or "No documentation available."

    # Create basic examples based on function name
    examples = get_examples(func_name)

    content = f'''"""
{func_name}() - Python Built-in Function

Description:
{doc}
"""

# Examples:

{examples}
'''
    return content

def get_examples(func_name):
    """Generate example code for common built-in functions"""

    examples_map = {
        'abs': '''# Returns absolute value of a number
result = abs(-5)
print(result)  # 5

result = abs(-3.14)
print(result)  # 3.14''',

        'all': '''# Returns True if all elements are true
result = all([True, True, True])
print(result)  # True

result = all([True, False, True])
print(result)  # False

result = all([1, 2, 3])
print(result)  # True''',

        'any': '''# Returns True if any element is true
result = any([False, False, True])
print(result)  # True

result = any([False, False, False])
print(result)  # False''',

        'ascii': '''# Returns printable representation of an object
result = ascii('Hello')
print(result)  # 'Hello'

result = ascii('Héllo')
print(result)  # 'H\\xe9llo' ''',

        'bin': '''# Converts integer to binary string
result = bin(10)
print(result)  # '0b1010'

result = bin(255)
print(result)  # '0b11111111' ''',

        'bool': '''# Converts value to Boolean
result = bool(1)
print(result)  # True

result = bool(0)
print(result)  # False

result = bool([])
print(result)  # False

result = bool([1, 2])
print(result)  # True''',

        'breakpoint': '''# Invokes debugger
# breakpoint()  # Uncomment to invoke debugger
pass''',

        'bytearray': '''# Creates mutable array of bytes
result = bytearray([65, 66, 67])
print(result)  # bytearray(b'ABC')

result = bytearray('hello', 'utf-8')
print(result)  # bytearray(b'hello')''',

        'bytes': '''# Creates immutable bytes object
result = bytes([65, 66, 67])
print(result)  # b'ABC'

result = bytes('hello', 'utf-8')
print(result)  # b'hello' ''',

        'callable': '''# Checks if object is callable
def my_function():
    pass

result = callable(my_function)
print(result)  # True

result = callable(42)
print(result)  # False''',

        'chr': '''# Returns character from Unicode code point
result = chr(65)
print(result)  # 'A'

result = chr(8364)
print(result)  # '€' ''',

        'classmethod': '''# Decorator for class methods
class MyClass:
    value = 10

    @classmethod
    def class_method(cls):
        return cls.value

result = MyClass.class_method()
print(result)  # 10''',

        'compile': '''# Compiles source into code object
code = compile('print("Hello")', '<string>', 'exec')
exec(code)  # Hello''',

        'complex': '''# Creates complex number
result = complex(2, 3)
print(result)  # (2+3j)

result = complex('3+4j')
print(result)  # (3+4j)''',

        'delattr': '''# Deletes attribute from object
class MyClass:
    attr = 10

obj = MyClass()
delattr(obj, 'attr')
# print(obj.attr)  # Would raise AttributeError''',

        'dict': '''# Creates dictionary
result = dict(a=1, b=2, c=3)
print(result)  # {'a': 1, 'b': 2, 'c': 3}

result = dict([('x', 100), ('y', 200)])
print(result)  # {'x': 100, 'y': 200}''',

        'dir': '''# Returns list of attributes
result = dir([])
print('append' in result)  # True

class MyClass:
    x = 10

result = dir(MyClass)
print('x' in result)  # True''',

        'divmod': '''# Returns quotient and remainder
result = divmod(10, 3)
print(result)  # (3, 1)

quotient, remainder = divmod(17, 5)
print(f"Quotient: {quotient}, Remainder: {remainder}")  # Quotient: 3, Remainder: 2''',

        'enumerate': '''# Returns enumerate object with index and value
fruits = ['apple', 'banana', 'cherry']
for index, fruit in enumerate(fruits):
    print(f"{index}: {fruit}")
# 0: apple
# 1: banana
# 2: cherry

for index, fruit in enumerate(fruits, start=1):
    print(f"{index}: {fruit}")''',

        'eval': '''# Evaluates string expression
result = eval('2 + 3')
print(result)  # 5

x = 10
result = eval('x * 2')
print(result)  # 20''',

        'exec': '''# Executes Python code dynamically
exec('x = 5')
exec('print(x)')  # 5

code = """
for i in range(3):
    print(i)
"""
exec(code)''',

        'filter': '''# Filters sequence based on function
numbers = [1, 2, 3, 4, 5, 6]
even_numbers = list(filter(lambda x: x % 2 == 0, numbers))
print(even_numbers)  # [2, 4, 6]

def is_positive(n):
    return n > 0

result = list(filter(is_positive, [-2, -1, 0, 1, 2]))
print(result)  # [1, 2]''',

        'float': '''# Converts to floating point number
result = float(5)
print(result)  # 5.0

result = float('3.14')
print(result)  # 3.14

result = float('inf')
print(result)  # inf''',

        'format': '''# Formats value
result = format(123.456, '.2f')
print(result)  # '123.46'

result = format(42, 'b')
print(result)  # '101010'

result = format(1000000, ',')
print(result)  # '1,000,000' ''',

        'frozenset': '''# Creates immutable set
result = frozenset([1, 2, 3, 2, 1])
print(result)  # frozenset({1, 2, 3})

set1 = frozenset(['a', 'b', 'c'])
set2 = frozenset(['b', 'c', 'd'])
print(set1 & set2)  # frozenset({'b', 'c'})''',

        'getattr': '''# Gets attribute value from object
class MyClass:
    x = 10
    y = 20

obj = MyClass()
result = getattr(obj, 'x')
print(result)  # 10

result = getattr(obj, 'z', 'default')
print(result)  # 'default' ''',

        'globals': '''# Returns dictionary of global symbol table
x = 10
result = globals()
print('x' in result)  # True
print(result['x'])  # 10''',

        'hasattr': '''# Checks if object has attribute
class MyClass:
    x = 10

obj = MyClass()
result = hasattr(obj, 'x')
print(result)  # True

result = hasattr(obj, 'y')
print(result)  # False''',

        'hash': '''# Returns hash value of object
result = hash('hello')
print(result)

result = hash((1, 2, 3))
print(result)

# Lists are not hashable
# hash([1, 2, 3])  # TypeError''',

        'help': '''# Invokes built-in help system
# help(print)  # Displays help for print function
# help(str)    # Displays help for string class
pass''',

        'hex': '''# Converts integer to hexadecimal string
result = hex(255)
print(result)  # '0xff'

result = hex(16)
print(result)  # '0x10' ''',

        'id': '''# Returns identity of object
x = [1, 2, 3]
result = id(x)
print(result)  # Unique identifier

y = x
print(id(y) == id(x))  # True (same object)''',

        'input': '''# Reads line from input
# name = input("Enter your name: ")
# print(f"Hello, {name}")

# Commented out to avoid blocking during automated runs
pass''',

        'int': '''# Converts to integer
result = int('42')
print(result)  # 42

result = int(3.14)
print(result)  # 3

result = int('1010', 2)  # Binary to decimal
print(result)  # 10

result = int('FF', 16)  # Hex to decimal
print(result)  # 255''',

        'isinstance': '''# Checks if object is instance of class
result = isinstance(5, int)
print(result)  # True

result = isinstance('hello', str)
print(result)  # True

result = isinstance([1, 2], (list, tuple))
print(result)  # True''',

        'issubclass': '''# Checks if class is subclass of another
result = issubclass(bool, int)
print(result)  # True

result = issubclass(int, bool)
print(result)  # False

class Parent:
    pass

class Child(Parent):
    pass

result = issubclass(Child, Parent)
print(result)  # True''',

        'iter': '''# Returns iterator object
my_list = [1, 2, 3]
iterator = iter(my_list)
print(next(iterator))  # 1
print(next(iterator))  # 2
print(next(iterator))  # 3''',

        'len': '''# Returns length of object
result = len([1, 2, 3, 4])
print(result)  # 4

result = len('hello')
print(result)  # 5

result = len({'a': 1, 'b': 2})
print(result)  # 2''',

        'list': '''# Creates list
result = list((1, 2, 3))
print(result)  # [1, 2, 3]

result = list('hello')
print(result)  # ['h', 'e', 'l', 'l', 'o']

result = list(range(5))
print(result)  # [0, 1, 2, 3, 4]''',

        'locals': '''# Returns dictionary of local symbol table
def my_function():
    x = 10
    y = 20
    result = locals()
    print(result)  # {'x': 10, 'y': 20}

my_function()''',

        'map': '''# Applies function to all items
numbers = [1, 2, 3, 4]
squared = list(map(lambda x: x**2, numbers))
print(squared)  # [1, 4, 9, 16]

def double(x):
    return x * 2

result = list(map(double, [1, 2, 3]))
print(result)  # [2, 4, 6]''',

        'max': '''# Returns maximum value
result = max(1, 5, 3, 9, 2)
print(result)  # 9

result = max([10, 20, 30])
print(result)  # 30

result = max('apple', 'banana', 'cherry')
print(result)  # 'cherry' ''',

        'memoryview': '''# Returns memory view object
data = bytearray(b'hello')
view = memoryview(data)
print(view[0])  # 104

print(bytes(view))  # b'hello' ''',

        'min': '''# Returns minimum value
result = min(1, 5, 3, 9, 2)
print(result)  # 1

result = min([10, 20, 30])
print(result)  # 10

result = min('apple', 'banana', 'cherry')
print(result)  # 'apple' ''',

        'next': '''# Gets next item from iterator
my_iter = iter([1, 2, 3])
result = next(my_iter)
print(result)  # 1

result = next(my_iter)
print(result)  # 2

result = next(my_iter, 'default')
print(result)  # 3''',

        'object': '''# Base class for all classes
obj = object()
print(type(obj))  # <class 'object'>

class MyClass(object):
    pass

instance = MyClass()
print(isinstance(instance, object))  # True''',

        'oct': '''# Converts integer to octal string
result = oct(8)
print(result)  # '0o10'

result = oct(64)
print(result)  # '0o100' ''',

        'open': '''# Opens file
# Write to file
# with open('test.txt', 'w') as file:
#     file.write('Hello, World!')

# Read from file
# with open('test.txt', 'r') as file:
#     content = file.read()
#     print(content)

# Commented to avoid file system changes
pass''',

        'ord': '''# Returns Unicode code point of character
result = ord('A')
print(result)  # 65

result = ord('€')
print(result)  # 8364''',

        'pow': '''# Returns power of number
result = pow(2, 3)
print(result)  # 8

result = pow(2, 3, 5)  # (2^3) % 5
print(result)  # 3''',

        'print': '''# Prints to console
print('Hello, World!')

print('Multiple', 'arguments', sep='-')
# Multiple-arguments

print('No newline', end='')
print(' continues')
# No newline continues''',

        'property': '''# Creates property
class Circle:
    def __init__(self, radius):
        self._radius = radius

    @property
    def radius(self):
        return self._radius

    @radius.setter
    def radius(self, value):
        if value < 0:
            raise ValueError("Radius cannot be negative")
        self._radius = value

circle = Circle(5)
print(circle.radius)  # 5
circle.radius = 10
print(circle.radius)  # 10''',

        'range': '''# Creates sequence of numbers
result = list(range(5))
print(result)  # [0, 1, 2, 3, 4]

result = list(range(2, 8))
print(result)  # [2, 3, 4, 5, 6, 7]

result = list(range(0, 10, 2))
print(result)  # [0, 2, 4, 6, 8]''',

        'repr': '''# Returns printable representation
result = repr('hello')
print(result)  # "'hello'"

result = repr([1, 2, 3])
print(result)  # '[1, 2, 3]' ''',

        'reversed': '''# Returns reversed iterator
result = list(reversed([1, 2, 3, 4]))
print(result)  # [4, 3, 2, 1]

result = list(reversed('hello'))
print(result)  # ['o', 'l', 'l', 'e', 'h']''',

        'round': '''# Rounds number to n digits
result = round(3.14159, 2)
print(result)  # 3.14

result = round(2.5)
print(result)  # 2 (rounds to even)

result = round(3.5)
print(result)  # 4''',

        'set': '''# Creates set
result = set([1, 2, 3, 2, 1])
print(result)  # {1, 2, 3}

result = set('hello')
print(result)  # {'h', 'e', 'l', 'o'}''',

        'setattr': '''# Sets attribute on object
class MyClass:
    pass

obj = MyClass()
setattr(obj, 'x', 10)
print(obj.x)  # 10

setattr(obj, 'y', 20)
print(obj.y)  # 20''',

        'slice': '''# Creates slice object
my_list = [0, 1, 2, 3, 4, 5]
s = slice(1, 5, 2)
result = my_list[s]
print(result)  # [1, 3]

s = slice(None, None, -1)
result = my_list[s]
print(result)  # [5, 4, 3, 2, 1, 0]''',

        'sorted': '''# Returns sorted list
result = sorted([3, 1, 4, 1, 5])
print(result)  # [1, 1, 3, 4, 5]

result = sorted(['banana', 'apple', 'cherry'])
print(result)  # ['apple', 'banana', 'cherry']

result = sorted([3, 1, 4], reverse=True)
print(result)  # [4, 3, 1]''',

        'staticmethod': '''# Decorator for static methods
class MyClass:
    @staticmethod
    def static_method():
        return "This is a static method"

result = MyClass.static_method()
print(result)

obj = MyClass()
result = obj.static_method()
print(result)''',

        'str': '''# Converts to string
result = str(42)
print(result)  # '42'

result = str([1, 2, 3])
print(result)  # '[1, 2, 3]'

result = str(3.14)
print(result)  # '3.14' ''',

        'sum': '''# Sums items of iterable
result = sum([1, 2, 3, 4])
print(result)  # 10

result = sum([1.5, 2.5, 3.0])
print(result)  # 7.0

result = sum([1, 2, 3], 10)  # Start value
print(result)  # 16''',

        'super': '''# Returns proxy object for parent class
class Parent:
    def method(self):
        return "Parent method"

class Child(Parent):
    def method(self):
        parent_result = super().method()
        return f"Child method, {parent_result}"

child = Child()
print(child.method())''',

        'tuple': '''# Creates tuple
result = tuple([1, 2, 3])
print(result)  # (1, 2, 3)

result = tuple('hello')
print(result)  # ('h', 'e', 'l', 'l', 'o')

result = tuple()
print(result)  # ()''',

        'type': '''# Returns type of object
result = type(42)
print(result)  # <class 'int'>

result = type('hello')
print(result)  # <class 'str'>

result = type([1, 2, 3])
print(result)  # <class 'list'>''',

        'vars': '''# Returns __dict__ attribute
class MyClass:
    def __init__(self):
        self.x = 10
        self.y = 20

obj = MyClass()
result = vars(obj)
print(result)  # {'x': 10, 'y': 20}''',

        'zip': '''# Combines iterables
names = ['Alice', 'Bob', 'Charlie']
ages = [25, 30, 35]
result = list(zip(names, ages))
print(result)  # [('Alice', 25), ('Bob', 30), ('Charlie', 35)]

for name, age in zip(names, ages):
    print(f"{name} is {age} years old")''',

        '__import__': '''# Imports module dynamically
# math_module = __import__('math')
# print(math_module.pi)  # 3.141592653589793

# Generally prefer 'import' statement over __import__
pass'''
    }

    return examples_map.get(func_name, f'''# Example usage of {func_name}()
# Add your examples here

result = {func_name}()
print(result)''')

# Generate files
created_count = 0
for func_name in builtin_functions:
    file_path = os.path.join(functions_dir, f"{func_name}.py")
    content = get_function_content(func_name)

    with open(file_path, 'w') as f:
        f.write(content)

    created_count += 1
    if created_count % 10 == 0:
        print(f"Created {created_count} files...")

print(f"\nSuccessfully created {created_count} files in '{functions_dir}/' directory")
print(f"\nBuilt-in functions covered:")
for i, func in enumerate(builtin_functions, 1):
    print(f"{i:2d}. {func}")
