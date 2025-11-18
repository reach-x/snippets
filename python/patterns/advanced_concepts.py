"""
Python Interview Questions: Advanced Concepts
==============================================
Topics: Decorators, Generators, Iterators, Context Managers, Comprehensions,
        Lambda, GIL, Memory Management, Closures
Greppable format for quick reference
"""

# ============================================================================
# DECORATORS
# ============================================================================

# Q: What is a decorator?
# A: A function that takes another function and extends its behavior without modifying it
def my_decorator(func):
    def wrapper():
        print("Before function")
        func()
        print("After function")
    return wrapper

@my_decorator
def say_hello():
    print("Hello!")

# say_hello() outputs:
# Before function
# Hello!
# After function

# Q: How do you create a decorator that accepts arguments?
# A: Create decorator with arguments in function being decorated
def repeat(times):
    def decorator(func):
        def wrapper(*args, **kwargs):
            for _ in range(times):
                result = func(*args, **kwargs)
            return result
        return wrapper
    return decorator

@repeat(3)
def greet(name):
    print(f"Hello {name}")

# Q: How do you preserve function metadata in decorators?
# A: Use functools.wraps
from functools import wraps

def my_decorator(func):
    @wraps(func)
    def wrapper(*args, **kwargs):
        return func(*args, **kwargs)
    return wrapper

# Q: What are common built-in decorators?
# A: @staticmethod, @classmethod, @property, @abstractmethod
class Example:
    @staticmethod
    def static_method():
        return "Static"

    @classmethod
    def class_method(cls):
        return "Class"

    @property
    def prop(self):
        return "Property"

# Q: How do you create a timing decorator?
# A: Measure time before and after function execution
import time

def timer(func):
    @wraps(func)
    def wrapper(*args, **kwargs):
        start = time.time()
        result = func(*args, **kwargs)
        end = time.time()
        print(f"{func.__name__} took {end - start:.4f} seconds")
        return result
    return wrapper

@timer
def slow_function():
    time.sleep(1)

# Q: How do you create a caching decorator?
# A: Use dictionary to store previous results
def memoize(func):
    cache = {}
    @wraps(func)
    def wrapper(*args):
        if args not in cache:
            cache[args] = func(*args)
        return cache[args]
    return wrapper

@memoize
def fibonacci(n):
    if n < 2:
        return n
    return fibonacci(n-1) + fibonacci(n-2)

# Q: What is functools.lru_cache?
# A: Built-in memoization decorator with LRU (Least Recently Used) cache
from functools import lru_cache

@lru_cache(maxsize=128)
def fibonacci_cached(n):
    if n < 2:
        return n
    return fibonacci_cached(n-1) + fibonacci_cached(n-2)

# ============================================================================
# GENERATORS
# ============================================================================

# Q: What is a generator?
# A: Function that yields values one at a time, pausing between yields
def count_up_to(n):
    count = 1
    while count <= n:
        yield count
        count += 1

for num in count_up_to(5):
    print(num)  # 1, 2, 3, 4, 5

# Q: What is the difference between return and yield?
# A: return ends function, yield pauses and resumes, maintaining state
def generator_example():
    yield 1
    yield 2
    yield 3

gen = generator_example()
print(next(gen))  # 1
print(next(gen))  # 2
print(next(gen))  # 3
# next(gen)  # Raises StopIteration

# Q: What are advantages of generators?
# A: 1) Memory efficient (lazy evaluation), 2) Can represent infinite sequences
def infinite_sequence():
    num = 0
    while True:
        yield num
        num += 1

# Q: How do you create a generator expression?
# A: Like list comprehension but with parentheses
squares_gen = (x**2 for x in range(10))  # Generator
squares_list = [x**2 for x in range(10)]  # List

# Q: What is yield from?
# A: Delegates to another generator
def generator_1():
    yield 1
    yield 2

def generator_2():
    yield from generator_1()
    yield 3
    yield 4

list(generator_2())  # [1, 2, 3, 4]

# Q: How do you send values to a generator?
# A: Use send() method
def echo():
    while True:
        received = yield
        print(f"Received: {received}")

gen = echo()
next(gen)  # Prime the generator
gen.send("Hello")  # Sends "Hello"

# Q: What is generator.close()?
# A: Terminates the generator
def my_gen():
    try:
        while True:
            yield 1
    except GeneratorExit:
        print("Generator closed")

# ============================================================================
# ITERATORS
# ============================================================================

# Q: What is an iterator?
# A: Object with __iter__() and __next__() methods
class CountDown:
    def __init__(self, start):
        self.current = start

    def __iter__(self):
        return self

    def __next__(self):
        if self.current <= 0:
            raise StopIteration
        self.current -= 1
        return self.current + 1

for num in CountDown(5):
    print(num)  # 5, 4, 3, 2, 1

# Q: What is the difference between iterator and iterable?
# A: Iterable has __iter__(), iterator has __iter__() and __next__()
# All iterators are iterables, but not all iterables are iterators

# Q: How do you use iter() and next()?
# A: iter() creates iterator from iterable, next() gets next value
my_list = [1, 2, 3]
my_iter = iter(my_list)
print(next(my_iter))  # 1
print(next(my_iter))  # 2
print(next(my_iter))  # 3
# next(my_iter)  # Raises StopIteration

# ============================================================================
# COMPREHENSIONS
# ============================================================================

# Q: What is list comprehension?
# A: Concise way to create lists
squares = [x**2 for x in range(10)]
even_squares = [x**2 for x in range(10) if x % 2 == 0]

# Q: What is dictionary comprehension?
# A: Concise way to create dictionaries
square_dict = {x: x**2 for x in range(5)}
filtered_dict = {k: v for k, v in square_dict.items() if v > 5}

# Q: What is set comprehension?
# A: Concise way to create sets
even_set = {x for x in range(10) if x % 2 == 0}

# Q: Can you do nested comprehensions?
# A: Yes, for nested iterations
matrix = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
flattened = [num for row in matrix for num in row]
# [1, 2, 3, 4, 5, 6, 7, 8, 9]

# Q: What is the difference between list comprehension and generator expression?
# A: List comprehension creates list in memory, generator is lazy
list_comp = [x**2 for x in range(1000000)]  # Creates list immediately
gen_exp = (x**2 for x in range(1000000))    # Creates generator, lazy

# ============================================================================
# LAMBDA FUNCTIONS
# ============================================================================

# Q: What is a lambda function?
# A: Anonymous one-line function
add = lambda x, y: x + y
print(add(3, 5))  # 8

# Q: When to use lambda vs regular function?
# A: Use lambda for simple operations, especially with map, filter, sorted
numbers = [1, 2, 3, 4, 5]
squared = list(map(lambda x: x**2, numbers))
evens = list(filter(lambda x: x % 2 == 0, numbers))
sorted_by_abs = sorted([-4, -1, 3, 2], key=lambda x: abs(x))

# Q: Can lambda have multiple statements?
# A: No, lambda is limited to single expression
# For complex logic, use regular function

# ============================================================================
# CLOSURES
# ============================================================================

# Q: What is a closure?
# A: Function that remembers values from enclosing scope
def outer(x):
    def inner(y):
        return x + y
    return inner

add_5 = outer(5)
print(add_5(3))  # 8
print(add_5(10))  # 15

# Q: How do closures work?
# A: Inner function captures variables from outer function's scope
def make_multiplier(n):
    def multiply(x):
        return x * n
    return multiply

times_3 = make_multiplier(3)
times_5 = make_multiplier(5)
print(times_3(10))  # 30
print(times_5(10))  # 50

# Q: What is the nonlocal keyword?
# A: Allows modifying variable from enclosing scope
def counter():
    count = 0
    def increment():
        nonlocal count
        count += 1
        return count
    return increment

count_up = counter()
print(count_up())  # 1
print(count_up())  # 2
print(count_up())  # 3

# ============================================================================
# CONTEXT MANAGERS
# ============================================================================

# Q: What is a context manager?
# A: Object that defines __enter__ and __exit__ for resource management
# Use with 'with' statement

# Q: Why use context managers?
# A: Ensure resources are properly cleaned up (files, locks, connections)
with open('file.txt', 'w') as f:
    f.write('Hello')
# File automatically closed after with block

# Q: How do you create a context manager with contextlib?
# A: Use @contextmanager decorator
from contextlib import contextmanager

@contextmanager
def my_context():
    print("Enter")
    yield "Resource"
    print("Exit")

with my_context() as resource:
    print(f"Using {resource}")

# Q: How do you handle exceptions in context managers?
# A: __exit__ receives exception info
class MyContext:
    def __enter__(self):
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        if exc_type is not None:
            print(f"Exception occurred: {exc_val}")
        return False  # Don't suppress exception

# ============================================================================
# ARGS AND KWARGS
# ============================================================================

# Q: What is *args?
# A: Allows function to accept any number of positional arguments
def sum_all(*args):
    return sum(args)

print(sum_all(1, 2, 3, 4, 5))  # 15

# Q: What is **kwargs?
# A: Allows function to accept any number of keyword arguments
def print_info(**kwargs):
    for key, value in kwargs.items():
        print(f"{key}: {value}")

print_info(name="Alice", age=30, city="NYC")

# Q: Can you combine args and kwargs?
# A: Yes, order is: regular args, *args, **kwargs
def complex_function(a, b, *args, **kwargs):
    print(f"a={a}, b={b}")
    print(f"args={args}")
    print(f"kwargs={kwargs}")

complex_function(1, 2, 3, 4, x=5, y=6)

# Q: How do you unpack arguments?
# A: Use * for list/tuple, ** for dictionary
def add(a, b, c):
    return a + b + c

numbers = [1, 2, 3]
print(add(*numbers))  # Unpacks to add(1, 2, 3)

params = {'a': 1, 'b': 2, 'c': 3}
print(add(**params))  # Unpacks to add(a=1, b=2, c=3)

# ============================================================================
# PYTHON INTERNALS
# ============================================================================

# Q: What is the GIL (Global Interpreter Lock)?
# A: Lock that allows only one thread to execute Python bytecode at a time
# Affects CPU-bound multithreading, not I/O-bound

# Q: How does Python manage memory?
# A: Reference counting + garbage collection for circular references
import sys
a = []
print(sys.getrefcount(a))  # Number of references to object

# Q: What is garbage collection in Python?
# A: Automatic memory management that frees unused objects
import gc
gc.collect()  # Force garbage collection

# Q: What are shallow copy vs deep copy?
# A: Shallow copies reference, deep copies duplicate nested objects
import copy

original = [[1, 2, 3], [4, 5, 6]]
shallow = copy.copy(original)
deep = copy.deepcopy(original)

original[0][0] = 999
# shallow[0][0] is also 999 (shares reference)
# deep[0][0] is still 1 (independent copy)

# Q: What is the difference between is and ==?
# A: 'is' checks identity (same object), '==' checks equality (same value)
a = [1, 2, 3]
b = [1, 2, 3]
c = a

print(a == b)  # True (same values)
print(a is b)  # False (different objects)
print(a is c)  # True (same object)

# Q: What is object interning?
# A: Python caches small integers and short strings for efficiency
a = 256
b = 256
print(a is b)  # True (interned)

x = 257
y = 257
print(x is y)  # False (not interned, but may be True in interactive mode)

# ============================================================================
# EXCEPTION HANDLING
# ============================================================================

# Q: How do you handle exceptions?
# A: Use try-except blocks
try:
    result = 10 / 0
except ZeroDivisionError:
    print("Cannot divide by zero")
except Exception as e:
    print(f"Error: {e}")
else:
    print("No exceptions")
finally:
    print("Always executed")

# Q: How do you raise exceptions?
# A: Use raise keyword
def divide(a, b):
    if b == 0:
        raise ValueError("Divisor cannot be zero")
    return a / b

# Q: How do you create custom exceptions?
# A: Inherit from Exception class
class CustomError(Exception):
    def __init__(self, message):
        self.message = message
        super().__init__(self.message)

# raise CustomError("Something went wrong")

# Q: What is exception chaining?
# A: Raising exception while handling another
try:
    result = 1 / 0
except ZeroDivisionError as e:
    raise ValueError("Invalid operation") from e

# ============================================================================
# TYPE HINTS (Python 3.5+)
# ============================================================================

# Q: What are type hints?
# A: Optional static typing for better code documentation and IDE support
from typing import List, Dict, Tuple, Optional, Union

def greet(name: str) -> str:
    return f"Hello {name}"

def process_numbers(numbers: List[int]) -> int:
    return sum(numbers)

def get_user(user_id: int) -> Optional[Dict[str, str]]:
    # Returns dict or None
    return {"name": "Alice", "email": "alice@example.com"}

# Q: What is Union type?
# A: Allows multiple types
def process_value(value: Union[int, str]) -> str:
    return str(value)

# Q: What is Any type?
# A: Accepts any type (escape hatch)
from typing import Any

def flexible_function(value: Any) -> Any:
    return value

# ============================================================================
# ADVANCED BUILT-IN FUNCTIONS
# ============================================================================

# Q: What is enumerate()?
# A: Returns index and value while iterating
fruits = ['apple', 'banana', 'cherry']
for index, fruit in enumerate(fruits):
    print(f"{index}: {fruit}")

# Q: What is zip()?
# A: Combines multiple iterables element-wise
names = ['Alice', 'Bob', 'Charlie']
ages = [25, 30, 35]
for name, age in zip(names, ages):
    print(f"{name} is {age}")

# Q: What is map()?
# A: Applies function to each element
numbers = [1, 2, 3, 4, 5]
squared = list(map(lambda x: x**2, numbers))

# Q: What is filter()?
# A: Filters elements based on condition
evens = list(filter(lambda x: x % 2 == 0, numbers))

# Q: What is reduce()?
# A: Applies function cumulatively to reduce to single value
from functools import reduce
product = reduce(lambda x, y: x * y, numbers)  # 1*2*3*4*5 = 120

# Q: What is all() and any()?
# A: all() returns True if all elements are True, any() if at least one is True
print(all([True, True, True]))  # True
print(all([True, False, True]))  # False
print(any([False, False, True]))  # True
print(any([False, False, False]))  # False
