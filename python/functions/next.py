"""
next() - Python Built-in Function

Description:
next(iterator[, default])

Return the next item from the iterator. If default is given and the iterator
is exhausted, it is returned instead of raising StopIteration.
"""

# Examples:

# Gets next item from iterator
my_iter = iter([1, 2, 3])
result = next(my_iter)
print(result)  # 1

result = next(my_iter)
print(result)  # 2

result = next(my_iter, 'default')
print(result)  # 3
