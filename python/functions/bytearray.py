"""
bytearray() - Python Built-in Function

Description:
bytearray(iterable_of_ints) -> bytearray
bytearray(string, encoding[, errors]) -> bytearray
bytearray(bytes_or_buffer) -> mutable copy of bytes_or_buffer
bytearray(int) -> bytes array of size given by the parameter initialized with null bytes
bytearray() -> empty bytes array

Construct a mutable bytearray object from:
  - an iterable yielding integers in range(256)
  - a text string encoded using the specified encoding
  - a bytes or a buffer object
  - any object implementing the buffer API.
  - an integer
"""

# Examples:

# Creates mutable array of bytes
result = bytearray([65, 66, 67])
print(result)  # bytearray(b'ABC')

result = bytearray('hello', 'utf-8')
print(result)  # bytearray(b'hello')
