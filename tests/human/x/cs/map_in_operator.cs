using System;
using System.Collections.Generic;

class Program
{
    static void Main()
    {
        var m = new Dictionary<int, string> { { 1, "a" }, { 2, "b" } };
        Console.WriteLine(m.ContainsKey(1));
        Console.WriteLine(m.ContainsKey(3));
    }
}
