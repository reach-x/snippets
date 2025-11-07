defmodule PatternMatching do
  @moduledoc """
  Pattern matching and function examples in Elixir
  """

  # Pattern matching in function heads
  def factorial(0), do: 1
  def factorial(n) when n > 0, do: n * factorial(n - 1)

  # Pattern matching with lists
  def sum_list([]), do: 0
  def sum_list([head | tail]), do: head + sum_list(tail)

  # Pattern matching with tuples
  def describe_point({0, 0}), do: "Origin"
  def describe_point({0, y}), do: "On Y-axis at #{y}"
  def describe_point({x, 0}), do: "On X-axis at #{x}"
  def describe_point({x, y}), do: "Point at (#{x}, #{y})"

  # Pattern matching with maps
  def greet(%{name: name, age: age}) do
    "Hello #{name}, you are #{age} years old"
  end

  def greet(%{name: name}) do
    "Hello #{name}"
  end

  # Case expressions
  def check_value(value) do
    case value do
      0 -> "zero"
      n when n < 0 -> "negative"
      n when n > 0 -> "positive"
      _ -> "unknown"
    end
  end

  # Cond expressions (like else-if chains)
  def grade_score(score) do
    cond do
      score >= 90 -> "A"
      score >= 80 -> "B"
      score >= 70 -> "C"
      score >= 60 -> "D"
      true -> "F"
    end
  end

  # Anonymous functions
  def demo_anonymous do
    # Simple anonymous function
    add = fn a, b -> a + b end
    IO.puts("5 + 3 = #{add.(5, 3)}")

    # Short syntax for anonymous functions
    multiply = &(&1 * &2)
    IO.puts("4 * 6 = #{multiply.(4, 6)}")

    # Passing functions as arguments
    numbers = [1, 2, 3, 4, 5]
    doubled = Enum.map(numbers, fn x -> x * 2 end)
    IO.puts("Doubled: #{inspect(doubled)}")
  end

  # Higher-order functions
  def apply_twice(f, x) do
    f.(f.(x))
  end

  # Pipe operator
  def demo_pipe do
    result =
      "hello world"
      |> String.upcase()
      |> String.split()
      |> Enum.reverse()
      |> Enum.join(" ")

    IO.puts("Piped result: #{result}")
  end

  # Demo function
  def demo do
    IO.puts("=== Factorial ===")
    IO.puts("5! = #{factorial(5)}")

    IO.puts("\n=== Sum List ===")
    IO.puts("Sum of [1,2,3,4,5] = #{sum_list([1, 2, 3, 4, 5])}")

    IO.puts("\n=== Describe Point ===")
    IO.puts(describe_point({0, 0}))
    IO.puts(describe_point({0, 5}))
    IO.puts(describe_point({3, 0}))
    IO.puts(describe_point({3, 4}))

    IO.puts("\n=== Greet ===")
    IO.puts(greet(%{name: "Alice", age: 30}))
    IO.puts(greet(%{name: "Bob"}))

    IO.puts("\n=== Check Value ===")
    IO.puts("0 is #{check_value(0)}")
    IO.puts("-5 is #{check_value(-5)}")
    IO.puts("10 is #{check_value(10)}")

    IO.puts("\n=== Grade Score ===")
    IO.puts("Score 95: #{grade_score(95)}")
    IO.puts("Score 72: #{grade_score(72)}")
    IO.puts("Score 55: #{grade_score(55)}")

    IO.puts("\n=== Anonymous Functions ===")
    demo_anonymous()

    IO.puts("\n=== Higher Order Functions ===")
    increment = fn x -> x + 1 end
    IO.puts("Apply increment twice to 5: #{apply_twice(increment, 5)}")

    IO.puts("\n=== Pipe Operator ===")
    demo_pipe()
  end
end

# Run the demo
PatternMatching.demo()
