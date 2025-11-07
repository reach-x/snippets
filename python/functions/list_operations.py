#!/usr/bin/env python3

numbers = [1, 2, 3, 4, 5]
fruits = ['apple', 'banana', 'cherry']

print(f"Numbers: {numbers}")
print(f"Length: {len(numbers)}")
print(f"Sum: {sum(numbers)}")
print(f"Max: {max(numbers)}")
print(f"Min: {min(numbers)}")

numbers.append(6)
print(f"After append: {numbers}")

numbers.pop()
print(f"After pop: {numbers}")

squared = [x**2 for x in numbers]
print(f"Squared: {squared}")

filtered = [x for x in numbers if x % 2 == 0]
print(f"Even numbers: {filtered}")

combined = numbers + fruits
print(f"Combined: {combined}")
