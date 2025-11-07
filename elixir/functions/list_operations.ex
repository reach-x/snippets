defmodule ListOperations do
  @moduledoc """
  List operations in Elixir demonstrating functional programming patterns
  """

  def demo do
    # Create lists
    numbers = [1, 2, 3, 4, 5]
    IO.puts("Numbers: #{inspect(numbers)}")

    # List length
    IO.puts("Length: #{length(numbers)}")

    # Head and tail
    [head | tail] = numbers
    IO.puts("Head: #{head}")
    IO.puts("Tail: #{inspect(tail)}")

    # Prepend (cons operator)
    new_list = [0 | numbers]
    IO.puts("Prepended 0: #{inspect(new_list)}")

    # Append
    appended = numbers ++ [6, 7]
    IO.puts("Appended [6, 7]: #{inspect(appended)}")

    # Map - transform each element
    squared = Enum.map(numbers, fn x -> x * x end)
    IO.puts("Squared: #{inspect(squared)}")

    # Filter - keep only matching elements
    evens = Enum.filter(numbers, fn x -> rem(x, 2) == 0 end)
    IO.puts("Even numbers: #{inspect(evens)}")

    # Reduce (fold) - accumulate values
    sum = Enum.reduce(numbers, 0, fn x, acc -> x + acc end)
    IO.puts("Sum: #{sum}")

    # Find element
    found = Enum.find(numbers, fn x -> x > 3 end)
    IO.puts("First > 3: #{found}")

    # Check if any/all match condition
    any_greater = Enum.any?(numbers, fn x -> x > 4 end)
    IO.puts("Any > 4: #{any_greater}")

    all_positive = Enum.all?(numbers, fn x -> x > 0 end)
    IO.puts("All positive: #{all_positive}")

    # Sort
    unsorted = [3, 1, 4, 1, 5, 9, 2, 6]
    sorted = Enum.sort(unsorted)
    IO.puts("Sorted: #{inspect(sorted)}")

    # Reverse
    reversed = Enum.reverse(numbers)
    IO.puts("Reversed: #{inspect(reversed)}")

    # Take first n elements
    first_three = Enum.take(numbers, 3)
    IO.puts("First 3: #{inspect(first_three)}")

    # Zip two lists together
    letters = [:a, :b, :c, :d, :e]
    zipped = Enum.zip(numbers, letters)
    IO.puts("Zipped: #{inspect(zipped)}")

    # Flat map
    nested = [[1, 2], [3, 4], [5]]
    flattened = List.flatten(nested)
    IO.puts("Flattened: #{inspect(flattened)}")
  end
end

# Run the demo
ListOperations.demo()
