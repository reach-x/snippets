"""
bytes() - Python Built-in Function

Description:
bytes(iterable_of_ints) -> bytes
bytes(string, encoding[, errors]) -> bytes
bytes(bytes_or_buffer) -> immutable copy of bytes_or_buffer
bytes(int) -> bytes object of size given by the parameter initialized with null bytes
bytes() -> empty bytes object

Construct an immutable array of bytes from:
  - an iterable yielding integers in range(256)
  - a text string encoded using the specified encoding
  - any object implementing the buffer API.
  - an integer
"""

# Examples:

# Creates immutable bytes object
result = bytes([65, 66, 67])
print(result)  # b'ABC'

result = bytes('hello', 'utf-8')
print(result)  # b'hello' 
