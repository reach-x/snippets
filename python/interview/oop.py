"""
Python Interview Questions: Object-Oriented Programming (OOP)
==============================================================
Topics: Classes, Inheritance, Polymorphism, Encapsulation, Abstraction
Greppable format for quick reference
"""

# ============================================================================
# CLASSES AND OBJECTS
# ============================================================================

# Q: What is a class?
# A: A blueprint for creating objects that defines attributes and methods
class Person:
    def __init__(self, name, age):
        self.name = name
        self.age = age

    def introduce(self):
        return f"My name is {self.name} and I am {self.age} years old"

# Q: How do you create an object/instance?
# A: Call the class like a function
person1 = Person("Alice", 30)
person2 = Person("Bob", 25)

# Q: What is __init__ method?
# A: Constructor method called when object is created, initializes attributes
# See Person class above

# Q: What is self?
# A: Reference to the current instance of the class
# It's the first parameter of instance methods

# Q: What are class variables vs instance variables?
# A: Class variables are shared by all instances, instance variables are unique per instance
class Dog:
    species = "Canis familiaris"  # Class variable

    def __init__(self, name, age):
        self.name = name  # Instance variable
        self.age = age    # Instance variable

dog1 = Dog("Buddy", 3)
dog2 = Dog("Max", 5)
# dog1.species == dog2.species (both "Canis familiaris")
# dog1.name != dog2.name (different values)

# Q: What are class methods?
# A: Methods that work with class rather than instance, use @classmethod decorator
class Pizza:
    def __init__(self, ingredients):
        self.ingredients = ingredients

    @classmethod
    def margherita(cls):
        return cls(["mozzarella", "tomatoes", "basil"])

    @classmethod
    def pepperoni(cls):
        return cls(["mozzarella", "tomatoes", "pepperoni"])

pizza = Pizza.margherita()

# Q: What are static methods?
# A: Methods that don't access instance or class, use @staticmethod decorator
class MathOperations:
    @staticmethod
    def add(x, y):
        return x + y

    @staticmethod
    def multiply(x, y):
        return x * y

result = MathOperations.add(5, 3)  # Can call without creating instance

# Q: What is the difference between class method and static method?
# A: Class method receives class as first parameter (cls), static method receives nothing
# Class method can access/modify class state, static method cannot

# ============================================================================
# ENCAPSULATION
# ============================================================================

# Q: What is encapsulation?
# A: Bundling data and methods together, restricting direct access to some components

# Q: How do you create private attributes in Python?
# A: Use double underscore prefix (name mangling) or single underscore (convention)
class BankAccount:
    def __init__(self, account_number, balance):
        self.account_number = account_number  # Public
        self._balance = balance  # Protected (convention)
        self.__pin = "1234"  # Private (name mangling)

    def get_balance(self):
        return self._balance

    def deposit(self, amount):
        if amount > 0:
            self._balance += amount

    def withdraw(self, amount):
        if 0 < amount <= self._balance:
            self._balance -= amount
            return True
        return False

# Q: What are property decorators?
# A: Allow getter, setter, and deleter methods for attributes
class Temperature:
    def __init__(self, celsius):
        self._celsius = celsius

    @property
    def celsius(self):
        return self._celsius

    @celsius.setter
    def celsius(self, value):
        if value < -273.15:
            raise ValueError("Temperature below absolute zero")
        self._celsius = value

    @property
    def fahrenheit(self):
        return (self._celsius * 9/5) + 32

    @fahrenheit.setter
    def fahrenheit(self, value):
        self.celsius = (value - 32) * 5/9

temp = Temperature(25)
print(temp.celsius)  # 25
print(temp.fahrenheit)  # 77.0
temp.fahrenheit = 86
print(temp.celsius)  # 30.0

# ============================================================================
# INHERITANCE
# ============================================================================

# Q: What is inheritance?
# A: Mechanism where a class acquires properties and methods from another class
class Animal:
    def __init__(self, name):
        self.name = name

    def speak(self):
        pass

class Dog(Animal):
    def speak(self):
        return f"{self.name} says Woof!"

class Cat(Animal):
    def speak(self):
        return f"{self.name} says Meow!"

dog = Dog("Buddy")
cat = Cat("Whiskers")

# Q: How do you call parent class constructor?
# A: Use super().__init__()
class Employee:
    def __init__(self, name, employee_id):
        self.name = name
        self.employee_id = employee_id

class Manager(Employee):
    def __init__(self, name, employee_id, department):
        super().__init__(name, employee_id)
        self.department = department

# Q: What is multiple inheritance?
# A: Class can inherit from multiple parent classes
class Flyer:
    def fly(self):
        return "Flying"

class Swimmer:
    def swim(self):
        return "Swimming"

class Duck(Flyer, Swimmer):
    pass

duck = Duck()
duck.fly()  # Works
duck.swim()  # Works

# Q: What is Method Resolution Order (MRO)?
# A: Order in which Python looks for methods in inheritance hierarchy
# Use ClassName.__mro__ or ClassName.mro() to see it
print(Duck.__mro__)
# (<class 'Duck'>, <class 'Flyer'>, <class 'Swimmer'>, <class 'object'>)

# Q: What is method overriding?
# A: Child class provides specific implementation of parent's method
class Shape:
    def area(self):
        pass

class Rectangle(Shape):
    def __init__(self, width, height):
        self.width = width
        self.height = height

    def area(self):
        return self.width * self.height

class Circle(Shape):
    def __init__(self, radius):
        self.radius = radius

    def area(self):
        return 3.14159 * self.radius ** 2

# ============================================================================
# POLYMORPHISM
# ============================================================================

# Q: What is polymorphism?
# A: Same interface for different data types, same method name different behaviors
def calculate_area(shape):
    return shape.area()

rectangle = Rectangle(5, 10)
circle = Circle(7)
print(calculate_area(rectangle))  # 50
print(calculate_area(circle))  # ~153.94

# Q: What is duck typing?
# A: If it walks like a duck and quacks like a duck, it's a duck
# Python doesn't check types, just if object has required methods
class Dog:
    def speak(self):
        return "Woof"

class Cat:
    def speak(self):
        return "Meow"

class Car:
    def drive(self):
        return "Vroom"

def make_it_speak(animal):
    return animal.speak()  # Works for any object with speak() method

make_it_speak(Dog())  # Works
make_it_speak(Cat())  # Works
# make_it_speak(Car())  # Raises AttributeError

# ============================================================================
# ABSTRACTION
# ============================================================================

# Q: What is abstraction?
# A: Hiding complex implementation details, showing only essential features
# Use ABC (Abstract Base Class) module
from abc import ABC, abstractmethod

class Vehicle(ABC):
    @abstractmethod
    def start_engine(self):
        pass

    @abstractmethod
    def stop_engine(self):
        pass

class Car(Vehicle):
    def start_engine(self):
        return "Car engine started"

    def stop_engine(self):
        return "Car engine stopped"

# vehicle = Vehicle()  # TypeError: Can't instantiate abstract class
car = Car()  # Works because Car implements all abstract methods

# Q: What is the difference between abstraction and encapsulation?
# A: Abstraction hides complexity, encapsulation hides data
# Abstraction: What it does (interface)
# Encapsulation: How it does (implementation)

# ============================================================================
# SPECIAL METHODS (MAGIC METHODS / DUNDER METHODS)
# ============================================================================

# Q: What are magic methods?
# A: Special methods with double underscores that Python calls implicitly

# Q: What is __str__ vs __repr__?
# A: __str__ is human-readable, __repr__ is unambiguous representation
class Point:
    def __init__(self, x, y):
        self.x = x
        self.y = y

    def __str__(self):
        return f"Point({self.x}, {self.y})"

    def __repr__(self):
        return f"Point(x={self.x}, y={self.y})"

point = Point(3, 4)
print(str(point))  # Point(3, 4)
print(repr(point))  # Point(x=3, y=4)

# Q: How do you make objects comparable?
# A: Implement __eq__, __lt__, __le__, __gt__, __ge__, __ne__
class Person:
    def __init__(self, name, age):
        self.name = name
        self.age = age

    def __eq__(self, other):
        return self.age == other.age

    def __lt__(self, other):
        return self.age < other.age

    def __le__(self, other):
        return self.age <= other.age

person1 = Person("Alice", 30)
person2 = Person("Bob", 25)
print(person1 > person2)  # True (30 > 25)

# Q: How do you make objects iterable?
# A: Implement __iter__ and __next__
class Counter:
    def __init__(self, max_value):
        self.max_value = max_value
        self.current = 0

    def __iter__(self):
        return self

    def __next__(self):
        if self.current < self.max_value:
            self.current += 1
            return self.current
        raise StopIteration

for num in Counter(5):
    print(num)  # Prints 1, 2, 3, 4, 5

# Q: How do you make objects callable?
# A: Implement __call__
class Multiplier:
    def __init__(self, factor):
        self.factor = factor

    def __call__(self, x):
        return x * self.factor

triple = Multiplier(3)
print(triple(10))  # 30

# Q: How do you implement operator overloading?
# A: Implement special methods like __add__, __sub__, __mul__, etc.
class Vector:
    def __init__(self, x, y):
        self.x = x
        self.y = y

    def __add__(self, other):
        return Vector(self.x + other.x, self.y + other.y)

    def __sub__(self, other):
        return Vector(self.x - other.x, self.y - other.y)

    def __mul__(self, scalar):
        return Vector(self.x * scalar, self.y * scalar)

    def __str__(self):
        return f"Vector({self.x}, {self.y})"

v1 = Vector(2, 3)
v2 = Vector(1, 1)
print(v1 + v2)  # Vector(3, 4)
print(v1 - v2)  # Vector(1, 2)
print(v1 * 3)   # Vector(6, 9)

# Q: How do you implement context managers?
# A: Implement __enter__ and __exit__
class FileHandler:
    def __init__(self, filename, mode):
        self.filename = filename
        self.mode = mode
        self.file = None

    def __enter__(self):
        self.file = open(self.filename, self.mode)
        return self.file

    def __exit__(self, exc_type, exc_val, exc_tb):
        if self.file:
            self.file.close()
        return False

# with FileHandler("test.txt", "w") as f:
#     f.write("Hello")

# Q: What is __len__?
# A: Returns length of object when len() is called
class CustomList:
    def __init__(self, items):
        self.items = items

    def __len__(self):
        return len(self.items)

    def __getitem__(self, index):
        return self.items[index]

custom = CustomList([1, 2, 3, 4, 5])
print(len(custom))  # 5
print(custom[2])    # 3

# ============================================================================
# COMPOSITION VS INHERITANCE
# ============================================================================

# Q: What is composition?
# A: Building complex objects by combining simpler objects (HAS-A relationship)
class Engine:
    def start(self):
        return "Engine started"

    def stop(self):
        return "Engine stopped"

class Car:
    def __init__(self):
        self.engine = Engine()  # Composition: Car HAS-A Engine

    def start(self):
        return self.engine.start()

# Q: When to use composition vs inheritance?
# A: Use inheritance for IS-A relationship, composition for HAS-A relationship
# Composition is often more flexible and easier to maintain

# ============================================================================
# DATACLASSES (Python 3.7+)
# ============================================================================

# Q: What are dataclasses?
# A: Decorator to automatically generate __init__, __repr__, __eq__, etc.
from dataclasses import dataclass, field

@dataclass
class Product:
    name: str
    price: float
    quantity: int = 0
    tags: list = field(default_factory=list)

    def total_value(self):
        return self.price * self.quantity

product = Product("Laptop", 999.99, 5)
print(product)  # Automatically has __repr__
print(product.total_value())  # 4999.95

# Q: How do you make dataclass immutable?
# A: Use frozen=True parameter
@dataclass(frozen=True)
class Point:
    x: int
    y: int

point = Point(3, 4)
# point.x = 5  # Raises FrozenInstanceError

# ============================================================================
# DESIGN PATTERNS
# ============================================================================

# Q: What is Singleton pattern?
# A: Ensures only one instance of class exists
class Singleton:
    _instance = None

    def __new__(cls):
        if cls._instance is None:
            cls._instance = super().__new__(cls)
        return cls._instance

s1 = Singleton()
s2 = Singleton()
print(s1 is s2)  # True - same instance

# Q: What is Factory pattern?
# A: Creates objects without specifying exact class
class AnimalFactory:
    @staticmethod
    def create_animal(animal_type):
        if animal_type == "dog":
            return Dog("Buddy")
        elif animal_type == "cat":
            return Cat("Whiskers")
        else:
            raise ValueError("Unknown animal type")

animal = AnimalFactory.create_animal("dog")
