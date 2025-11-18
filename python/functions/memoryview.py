"""
memoryview() - Python Built-in Function

Description:
Create a new memoryview object which references the given object.
"""

# Examples:

# Returns memory view object
data = bytearray(b'hello')
view = memoryview(data)
print(view[0])  # 104

print(bytes(view))  # b'hello' 
