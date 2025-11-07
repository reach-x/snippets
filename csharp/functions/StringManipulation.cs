using System;

class StringManipulation
{
    static void Main()
    {
        string text = "  Hello, World!  ";

        Console.WriteLine($"Original: '{text}'");
        Console.WriteLine($"Upper: '{text.ToUpper()}'");
        Console.WriteLine($"Lower: '{text.ToLower()}'");
        Console.WriteLine($"Trim: '{text.Trim()}'");
        Console.WriteLine($"Replace: '{text.Replace("World", "C#")}'");
        Console.WriteLine($"Split: {string.Join(", ", text.Trim().Split(", "))}");
        Console.WriteLine($"Contains: {text.Contains("World")}");
        Console.WriteLine($"StartsWith: {text.Trim().StartsWith("Hello")}");
        Console.WriteLine($"EndsWith: {text.Trim().EndsWith("!")}");
        Console.WriteLine($"Length: {text.Length}");
        Console.WriteLine($"Substring: '{text.Trim().Substring(0, 5)}'");
    }
}
