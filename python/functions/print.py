"""
print() - Python Built-in Function

Description:
Prints the values to a stream, or to sys.stdout by default.

sep
  string inserted between values, default a space.
end
  string appended after the last value, default a newline.
file
  a file-like object (stream); defaults to the current sys.stdout.
flush
  whether to forcibly flush the stream.
"""

# Examples:

# Prints to console
print('Hello, World!')

print('Multiple', 'arguments', sep='-')
# Multiple-arguments

print('No newline', end='')
print(' continues')
# No newline continues
