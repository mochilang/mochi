using System;
using System.Collections.Generic;
using System.Linq;

class Program
{
    static void Main()
    {
        var nums = new[] { 1, 2, 3 };
        var letters = new[] { "A", "B" };
        var pairs = from n in nums from l in letters where n % 2 == 0 select new { n, l };
        Console.WriteLine("--- Even pairs ---");
        foreach (var p in pairs)
        {
            Console.WriteLine($"{p.n} {p.l}");
        }
    }
}
