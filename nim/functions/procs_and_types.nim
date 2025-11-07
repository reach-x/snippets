# Procedures and types in Nim

import strformat

echo "\n=== Procedures and Types in Nim ===\n"

# Simple procedure
proc greet(name: string): string =
  result = fmt"Hello, {name}!"

echo greet("Alice")

# Procedure with default arguments
proc power(x: int, n: int = 2): int =
  result = 1
  for i in 1..n:
    result *= x

echo "\nPower(3): ", power(3)
echo "Power(3, 3): ", power(3, 3)

# Recursive procedure
proc factorial(n: int): int =
  if n <= 1:
    return 1
  else:
    return n * factorial(n - 1)

echo "\nFactorial 5: ", factorial(5)

# Object type
type
  Point = object
    x, y: float

var p = Point(x: 3.0, y: 4.0)
echo "\nPoint: (", p.x, ", ", p.y, ")")

# Method on object
proc distance(p: Point): float =
  result = sqrt(p.x * p.x + p.y * p.y)

echo "Distance from origin: ", p.distance()

# Enum
type
  Color = enum
    Red, Green, Blue

var color = Red
echo "\nColor: ", color

# Generic procedure
proc identity[T](x: T): T =
  return x

echo "\nIdentity(42): ", identity(42)
echo "Identity(\"test\"): ", identity("test")
