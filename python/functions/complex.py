"""
complex() - Python Built-in Function

Description:
Create a complex number from a string or numbers.

If a string is given, parse it as a complex number.
If a single number is given, convert it to a complex number.
If the 'real' or 'imag' arguments are given, create a complex number
with the specified real and imaginary components.
"""

# Examples:

# Creates complex number
result = complex(2, 3)
print(result)  # (2+3j)

result = complex('3+4j')
print(result)  # (3+4j)
