defmodule StringManipulation do
  @moduledoc """
  String manipulation examples in Elixir
  """

  def demo do
    # String concatenation
    str1 = "Hello"
    str2 = "World"
    concatenated = str1 <> " " <> str2
    IO.puts("Concatenated: #{concatenated}")

    # String interpolation
    name = "Alice"
    greeting = "Hello, #{name}!"
    IO.puts(greeting)

    # String length
    IO.puts("Length: #{String.length(greeting)}")

    # Uppercase and lowercase
    IO.puts("Uppercase: #{String.upcase(greeting)}")
    IO.puts("Lowercase: #{String.downcase(greeting)}")

    # String slicing
    text = "Hello, World!"
    IO.puts("Slice [0..4]: #{String.slice(text, 0..4)}")

    # String splitting
    words = String.split(text, ", ")
    IO.puts("Split: #{inspect(words)}")

    # String trimming
    padded = "  hello  "
    IO.puts("Trimmed: '#{String.trim(padded)}'")

    # String replacement
    replaced = String.replace(text, "World", "Elixir")
    IO.puts("Replaced: #{replaced}")

    # Check if string contains substring
    contains = String.contains?(text, "World")
    IO.puts("Contains 'World': #{contains}")

    # Check if string starts with
    starts_with = String.starts_with?(text, "Hello")
    IO.puts("Starts with 'Hello': #{starts_with}")

    # Reverse string
    reversed = String.reverse(text)
    IO.puts("Reversed: #{reversed}")
  end
end

# Run the demo
StringManipulation.demo()
