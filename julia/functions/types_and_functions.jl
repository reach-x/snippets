#!/usr/bin/env julia
# Types and functions in Julia

println("\n=== Types and Functions in Julia ===\n")

# Simple function
function greet(name)
    return "Hello, $name!"
end

println(greet("Alice"))

# Short-form function
square(x) = x^2
println("\nSquare 5: ", square(5))

# Multiple dispatch
area(radius::Float64) = π * radius^2
area(width::Float64, height::Float64) = width * height

println("\nCircle area: ", area(5.0))
println("Rectangle area: ", area(4.0, 6.0))

# Optional arguments
function power(x, n=2)
    return x^n
end

println("\nPower(3): ", power(3))
println("Power(3, 3): ", power(3, 3))

# Keyword arguments
function person(; name="Unknown", age=0)
    println("Name: $name, Age: $age")
end

person(name="Bob", age=30)

# Higher-order functions
function apply_twice(f, x)
    return f(f(x))
end

add1(x) = x + 1
println("\nApply twice (add1) to 5: ", apply_twice(add1, 5))

# Anonymous functions
doubled = map(x -> x * 2, [1, 2, 3, 4, 5])
println("Map (double): ", doubled)

# Recursion
function factorial(n)
    if n <= 1
        return 1
    else
        return n * factorial(n - 1)
    end
end

println("\nFactorial 5: ", factorial(5))

# Type annotations
function typed_sum(x::Int, y::Int)::Int
    return x + y
end

println("Typed sum: ", typed_sum(3, 4))

# Struct (composite type)
struct Point
    x::Float64
    y::Float64
end

p = Point(3.0, 4.0)
println("\nPoint: (", p.x, ", ", p.y, ")")

# Mutable struct
mutable struct Counter
    count::Int
end

function increment!(c::Counter)
    c.count += 1
end

counter = Counter(0)
increment!(counter)
increment!(counter)
println("Counter: ", counter.count)
