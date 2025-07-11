using System;
using System.Collections.Generic;
using System.Linq;

class Program {
    static void Main() {
        List<int> nums = new List<int> { 3, 1, 4 };
        Console.WriteLine(Enumerable.Min(nums));
        Console.WriteLine(Enumerable.Max(nums));
    }
}
