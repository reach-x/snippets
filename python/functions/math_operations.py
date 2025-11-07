#!/usr/bin/env python3

import math

numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

print(f"Sum: {sum(numbers)}")
print(f"Average: {sum(numbers) / len(numbers)}")
print(f"Max: {max(numbers)}")
print(f"Min: {min(numbers)}")

print(f"\nMath functions:")
print(f"Square root of 16: {math.sqrt(16)}")
print(f"Power 2^8: {math.pow(2, 8)}")
print(f"Ceiling of 4.2: {math.ceil(4.2)}")
print(f"Floor of 4.8: {math.floor(4.8)}")
print(f"Absolute value of -10: {abs(-10)}")

print(f"\nTrigonometry:")
print(f"Sin(90°): {math.sin(math.radians(90))}")
print(f"Cos(0°): {math.cos(math.radians(0))}")
print(f"Pi: {math.pi}")
print(f"e: {math.e}")

print(f"\nRounding:")
print(f"Round 3.14159 to 2 decimals: {round(3.14159, 2)}")
