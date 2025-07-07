using System;
using System.Collections.Generic;
using System.Text.Json;

class Program
{
    static void Main()
    {
        var people = new List<object>
        {
            new { name = "Alice", age = 30 },
            new { name = "Bob", age = 25 },
        };
        foreach (var p in people)
        {
            Console.WriteLine(JsonSerializer.Serialize(p));
        }
    }
}
