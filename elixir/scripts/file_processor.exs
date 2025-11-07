#!/usr/bin/env elixir

defmodule FileProcessor do
  @moduledoc """
  Script to demonstrate file processing in Elixir
  Reads a file, processes each line, and writes to output
  """

  def process_file(input_path, output_path) do
    case File.read(input_path) do
      {:ok, content} ->
        IO.puts("Reading file: #{input_path}")

        processed =
          content
          |> String.split("\n")
          |> Enum.with_index(1)
          |> Enum.map(fn {line, num} -> "#{num}: #{String.upcase(line)}" end)
          |> Enum.join("\n")

        case File.write(output_path, processed) do
          :ok ->
            IO.puts("Successfully wrote to: #{output_path}")
          {:error, reason} ->
            IO.puts("Error writing file: #{reason}")
        end

      {:error, reason} ->
        IO.puts("Error reading file: #{reason}")
    end
  end

  def count_words(file_path) do
    case File.read(file_path) do
      {:ok, content} ->
        word_count =
          content
          |> String.split(~r/\s+/)
          |> Enum.filter(&(&1 != ""))
          |> length()

        line_count =
          content
          |> String.split("\n")
          |> length()

        IO.puts("File: #{file_path}")
        IO.puts("Lines: #{line_count}")
        IO.puts("Words: #{word_count}")
        IO.puts("Characters: #{String.length(content)}")

      {:error, reason} ->
        IO.puts("Error: #{reason}")
    end
  end

  def demo do
    # Create a sample input file
    input_file = "../tmp/input.txt"
    output_file = "../tmp/output.txt"

    # Ensure tmp directory exists
    File.mkdir_p!("../tmp")

    # Write sample content
    sample_content = """
    Hello World
    This is a test file
    With multiple lines
    For processing
    """

    File.write!(input_file, sample_content)

    # Process the file
    IO.puts("=== Processing File ===")
    process_file(input_file, output_file)

    IO.puts("\n=== Counting Words ===")
    count_words(input_file)

    IO.puts("\n=== Output Content ===")
    case File.read(output_file) do
      {:ok, content} -> IO.puts(content)
      _ -> IO.puts("Could not read output")
    end
  end
end

# Run if executed as script
FileProcessor.demo()
