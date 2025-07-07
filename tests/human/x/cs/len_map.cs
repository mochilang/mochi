using System;
using System.Collections.Generic;

class Program
{
    static void Main()
    {
        var map = new Dictionary<string, int> { ["a"] = 1, ["b"] = 2 };
        Console.WriteLine(map.Count);
    }
}
