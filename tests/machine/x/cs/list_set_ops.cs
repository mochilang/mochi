using System;
using System.Collections.Generic;
using System.Linq;

class Program {
    static void Main() {
        Console.WriteLine("[" + string.Join(", ", Enumerable.Union(new List<int> { 1, 2 }, new List<int> { 2, 3 }).ToArray()) + "]");
        Console.WriteLine("[" + string.Join(", ", Enumerable.Except(new List<int> { 1, 2, 3 }, new List<int> { 2 }).ToArray()) + "]");
        Console.WriteLine("[" + string.Join(", ", Enumerable.Intersect(new List<int> { 1, 2, 3 }, new List<int> { 2, 4 }).ToArray()) + "]");
        Console.WriteLine(Enumerable.Concat(new List<int> { 1, 2 }, new List<int> { 2, 3 }).ToArray().Length);
    }
}
