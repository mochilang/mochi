using System;
using System.Collections.Generic;

class Program
{
    static void Main()
    {
        var m = new Dictionary<string, int> { { "a", 1 }, { "b", 2 } };
        Console.WriteLine(m.ContainsKey("a"));
        Console.WriteLine(m.ContainsKey("c"));
    }
}
