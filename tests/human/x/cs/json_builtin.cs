using System;
using System.Collections.Generic;
using System.Text.Json;

class Program
{
    static void Main()
    {
        var m = new Dictionary<string, int> { ["a"] = 1, ["b"] = 2 };
        string json = JsonSerializer.Serialize(m);
        Console.WriteLine(json);
    }
}
