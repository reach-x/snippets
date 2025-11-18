"""
pow() - Python Built-in Function

Description:
Equivalent to base**exp with 2 arguments or base**exp % mod with 3 arguments

Some types, such as ints, are able to use a more efficient algorithm when
invoked using the three argument form.
"""

# Examples:

# Returns power of number
result = pow(2, 3)
print(result)  # 8

result = pow(2, 3, 5)  # (2^3) % 5
print(result)  # 3
