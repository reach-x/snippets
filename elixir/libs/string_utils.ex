defmodule StringUtils do
  @moduledoc """
  Utility functions for string manipulation
  Reusable library for common string operations
  """

  @doc """
  Capitalizes the first letter of each word in a string

  ## Examples
      iex> StringUtils.title_case("hello world")
      "Hello World"
  """
  def title_case(string) do
    string
    |> String.split()
    |> Enum.map(&String.capitalize/1)
    |> Enum.join(" ")
  end

  @doc """
  Truncates a string to a specified length and adds ellipsis

  ## Examples
      iex> StringUtils.truncate("Hello World", 8)
      "Hello..."
  """
  def truncate(string, max_length) when is_binary(string) and is_integer(max_length) do
    if String.length(string) <= max_length do
      string
    else
      String.slice(string, 0, max_length - 3) <> "..."
    end
  end

  @doc """
  Slugifies a string for use in URLs

  ## Examples
      iex> StringUtils.slugify("Hello World!")
      "hello-world"
  """
  def slugify(string) do
    string
    |> String.downcase()
    |> String.replace(~r/[^\w\s-]/, "")
    |> String.replace(~r/\s+/, "-")
    |> String.trim("-")
  end

  @doc """
  Counts the occurrences of a substring in a string

  ## Examples
      iex> StringUtils.count_occurrences("hello world hello", "hello")
      2
  """
  def count_occurrences(string, substring) do
    string
    |> String.split(substring)
    |> length()
    |> Kernel.-(1)
  end

  @doc """
  Checks if a string is a palindrome

  ## Examples
      iex> StringUtils.palindrome?("racecar")
      true
      iex> StringUtils.palindrome?("hello")
      false
  """
  def palindrome?(string) do
    normalized =
      string
      |> String.downcase()
      |> String.replace(~r/[^\w]/, "")

    normalized == String.reverse(normalized)
  end

  @doc """
  Extracts all numbers from a string

  ## Examples
      iex> StringUtils.extract_numbers("There are 123 apples and 45 oranges")
      ["123", "45"]
  """
  def extract_numbers(string) do
    Regex.scan(~r/\d+/, string)
    |> List.flatten()
  end

  @doc """
  Wraps text to a specified line width

  ## Examples
      iex> StringUtils.word_wrap("This is a long sentence", 10)
      "This is a\\nlong\\nsentence"
  """
  def word_wrap(string, width) do
    string
    |> String.split()
    |> Enum.reduce({[], []}, fn word, {lines, current_line} ->
      line_length = current_line |> Enum.join(" ") |> String.length()
      word_length = String.length(word)

      if line_length + word_length + 1 > width and current_line != [] do
        {[Enum.join(current_line, " ") | lines], [word]}
      else
        {lines, [word | current_line]}
      end
    end)
    |> then(fn {lines, current_line} ->
      [Enum.join(current_line, " ") | lines]
    end)
    |> Enum.reverse()
    |> Enum.join("\n")
  end

  @doc """
  Removes duplicate consecutive characters

  ## Examples
      iex> StringUtils.remove_duplicate_chars("hellooo world")
      "helo world"
  """
  def remove_duplicate_chars(string) do
    string
    |> String.graphemes()
    |> Enum.chunk_by(&(&1))
    |> Enum.map(&hd/1)
    |> Enum.join()
  end

  @doc """
  Converts a string to camelCase

  ## Examples
      iex> StringUtils.camel_case("hello world example")
      "helloWorldExample"
  """
  def camel_case(string) do
    [first | rest] =
      string
      |> String.split(~r/[\s_-]+/)

    first = String.downcase(first)
    rest = Enum.map(rest, &String.capitalize/1)

    Enum.join([first | rest])
  end

  @doc """
  Converts a string to snake_case

  ## Examples
      iex> StringUtils.snake_case("HelloWorld")
      "hello_world"
  """
  def snake_case(string) do
    string
    |> String.replace(~r/([A-Z])/, "_\\1")
    |> String.downcase()
    |> String.trim("_")
    |> String.replace(~r/[\s-]+/, "_")
  end
end

# Demo usage
defmodule StringUtilsDemo do
  def run do
    IO.puts("=== String Utils Library Demo ===\n")

    IO.puts("Title Case:")
    IO.puts(StringUtils.title_case("hello world from elixir"))

    IO.puts("\nTruncate:")
    IO.puts(StringUtils.truncate("This is a very long string", 15))

    IO.puts("\nSlugify:")
    IO.puts(StringUtils.slugify("Hello World! This is Elixir"))

    IO.puts("\nCount Occurrences:")
    IO.puts("'hello' appears #{StringUtils.count_occurrences("hello world hello", "hello")} times")

    IO.puts("\nPalindrome:")
    IO.puts("'racecar' is palindrome: #{StringUtils.palindrome?("racecar")}")
    IO.puts("'hello' is palindrome: #{StringUtils.palindrome?("hello")}")

    IO.puts("\nExtract Numbers:")
    IO.puts(inspect(StringUtils.extract_numbers("I have 42 apples and 17 oranges")))

    IO.puts("\nWord Wrap (width 20):")
    IO.puts(StringUtils.word_wrap("This is a long sentence that needs to be wrapped", 20))

    IO.puts("\nRemove Duplicate Chars:")
    IO.puts(StringUtils.remove_duplicate_chars("hellooo  wooorrrld"))

    IO.puts("\nCamel Case:")
    IO.puts(StringUtils.camel_case("hello world example"))

    IO.puts("\nSnake Case:")
    IO.puts(StringUtils.snake_case("HelloWorldExample"))
  end
end

# Run demo if this file is executed
if __ENV__.file == :stdin || Path.basename(__ENV__.file) == "string_utils.ex" do
  StringUtilsDemo.run()
end
