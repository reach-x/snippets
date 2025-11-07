#!/usr/bin/env rdmd
// Array operations in D

import std.stdio;
import std.algorithm;
import std.array;
import std.range;

void main()
{
    writeln("\n=== Array Operations in D ===\n");

    // Create arrays
    int[] numbers = [1, 2, 3, 4, 5];
    string[] fruits = ["apple", "banana", "cherry"];

    writeln("Numbers: ", numbers);
    writeln("Fruits: ", fruits);

    // Array length
    writeln("\nLength: ", numbers.length);

    // Access elements
    writeln("First: ", numbers[0]);
    writeln("Last: ", numbers[$-1]);

    // Append
    numbers ~= 6;
    writeln("\nAfter ~= 6: ", numbers);

    numbers ~= [7, 8, 9];
    writeln("After ~= [7,8,9]: ", numbers);

    // Map
    auto squared = numbers.map!(x => x * x).array;
    writeln("\nMap (square): ", squared);

    // Filter
    auto evens = numbers.filter!(x => x % 2 == 0).array;
    writeln("Filter (even): ", evens);

    // Reduce
    auto sum = numbers.sum;
    writeln("\nSum: ", sum);

    // Sort
    int[] unsorted = [5, 2, 8, 1, 9, 3];
    sort(unsorted);
    writeln("\nSorted: ", unsorted);

    // Reverse
    writeln("Reversed: ", numbers.retro.array);

    // Unique
    int[] withDupes = [1, 2, 2, 3, 3, 3, 4];
    writeln("\nUniq: ", withDupes.uniq.array);

    // Slicing
    writeln("\nSlice [1..4]: ", numbers[1..4]);

    // Range
    writeln("Range iota(1, 11): ", iota(1, 11).array);

    // Array comprehension
    auto squares = iota(1, 6).map!(x => x * x).array;
    writeln("\nSquares: ", squares);
}
