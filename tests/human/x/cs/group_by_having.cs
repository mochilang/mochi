using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.Json;

class Person
{
    public string name;
    public string city;
}

class Program
{
    static void Main()
    {
        var people = new List<Person>
        {
            new Person { name = "Alice", city = "Paris" },
            new Person { name = "Bob", city = "Hanoi" },
            new Person { name = "Charlie", city = "Paris" },
            new Person { name = "Diana", city = "Hanoi" },
            new Person { name = "Eve", city = "Paris" },
            new Person { name = "Frank", city = "Hanoi" },
            new Person { name = "George", city = "Paris" },
        };

        var big = people
            .GroupBy(p => p.city)
            .Where(g => g.Count() >= 4)
            .Select(g => new { city = g.Key, num = g.Count() });

        string json = JsonSerializer.Serialize(big);
        Console.WriteLine(json);
    }
}
