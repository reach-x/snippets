"""
zip() - Python Built-in Function

Description:
The zip object yields n-length tuples, where n is the number of iterables
passed as positional arguments to zip().  The i-th element in every tuple
comes from the i-th iterable argument to zip().  This continues until the
shortest argument is exhausted.

If strict is true and one of the arguments is exhausted before the others,
raise a ValueError.

   >>> list(zip('abcdefg', range(3), range(4)))
   [('a', 0, 0), ('b', 1, 1), ('c', 2, 2)]
"""

# Examples:

# Combines iterables
names = ['Alice', 'Bob', 'Charlie']
ages = [25, 30, 35]
result = list(zip(names, ages))
print(result)  # [('Alice', 25), ('Bob', 30), ('Charlie', 35)]

for name, age in zip(names, ages):
    print(f"{name} is {age} years old")
