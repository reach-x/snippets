"""
int() - Python Built-in Function

Description:
int([x]) -> integer
int(x, base=10) -> integer

Convert a number or string to an integer, or return 0 if no arguments
are given.  If x is a number, return x.__int__().  For floating-point
numbers, this truncates towards zero.

If x is not a number or if base is given, then x must be a string,
bytes, or bytearray instance representing an integer literal in the
given base.  The literal can be preceded by '+' or '-' and be surrounded
by whitespace.  The base defaults to 10.  Valid bases are 0 and 2-36.
Base 0 means to interpret the base from the string as an integer literal.
>>> int('0b100', base=0)
4
"""

# Examples:

# Converts to integer
result = int('42')
print(result)  # 42

result = int(3.14)
print(result)  # 3

result = int('1010', 2)  # Binary to decimal
print(result)  # 10

result = int('FF', 16)  # Hex to decimal
print(result)  # 255
