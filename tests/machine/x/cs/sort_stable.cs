using System;
using System.Collections.Generic;
using System.Linq;

class Program
{
    static void Main()
    {
        var items = new dynamic[] { new Dictionary<string, dynamic> { { "n", 1 }, { "v", "a" } }, new Dictionary<string, dynamic> { { "n", 1 }, { "v", "b" } }, new Dictionary<string, dynamic> { { "n", 2 }, { "v", "c" } } };
        var result = items.OrderBy(i => i["n"]).Select(i => i["v"]).ToArray();
        Console.WriteLine(string.Join(" ", result));
    }
}
