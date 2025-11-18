"""
divmod() - Python Built-in Function

Description:
Return the tuple (x//y, x%y).  Invariant: div*y + mod == x.
"""

# Examples:

# Returns quotient and remainder
result = divmod(10, 3)
print(result)  # (3, 1)

quotient, remainder = divmod(17, 5)
print(f"Quotient: {quotient}, Remainder: {remainder}")  # Quotient: 3, Remainder: 2
