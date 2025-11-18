"""
format() - Python Built-in Function

Description:
Return type(value).__format__(value, format_spec)

Many built-in types implement format_spec according to the
Format Specification Mini-language. See help('FORMATTING').

If type(value) does not supply a method named __format__
and format_spec is empty, then str(value) is returned.
See also help('SPECIALMETHODS').
"""

# Examples:

# Formats value
result = format(123.456, '.2f')
print(result)  # '123.46'

result = format(42, 'b')
print(result)  # '101010'

result = format(1000000, ',')
print(result)  # '1,000,000' 
