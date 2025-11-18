"""
round() - Python Built-in Function

Description:
Round a number to a given precision in decimal digits.

The return value is an integer if ndigits is omitted or None.  Otherwise
the return value has the same type as the number.  ndigits may be negative.
"""

# Examples:

# Rounds number to n digits
result = round(3.14159, 2)
print(result)  # 3.14

result = round(2.5)
print(result)  # 2 (rounds to even)

result = round(3.5)
print(result)  # 4
