using System;
using System.Collections.Generic;

class Program
{
    static void Main()
    {
        int x = 3;
        int y = 4;
        var m = new Dictionary<string, int> { ["a"] = x, ["b"] = y };
        Console.WriteLine($"{m["a"]} {m["b"]}");
    }
}
