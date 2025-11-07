using System;
using System.Collections.Generic;
using System.Linq;

class ListOperations
{
    static void Main()
    {
        List<int> numbers = new List<int> { 1, 2, 3, 4, 5 };

        Console.WriteLine($"Numbers: {string.Join(", ", numbers)}");
        Console.WriteLine($"Count: {numbers.Count}");
        Console.WriteLine($"Sum: {numbers.Sum()}");
        Console.WriteLine($"Max: {numbers.Max()}");
        Console.WriteLine($"Min: {numbers.Min()}");

        numbers.Add(6);
        Console.WriteLine($"After Add: {string.Join(", ", numbers)}");

        numbers.RemoveAt(numbers.Count - 1);
        Console.WriteLine($"After Remove: {string.Join(", ", numbers)}");

        var squared = numbers.Select(x => x * x).ToList();
        Console.WriteLine($"Squared: {string.Join(", ", squared)}");

        var evens = numbers.Where(x => x % 2 == 0).ToList();
        Console.WriteLine($"Even numbers: {string.Join(", ", evens)}");
    }
}
