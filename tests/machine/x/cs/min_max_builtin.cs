using System;
using System.Linq;

class Program {
    static void Main() {
        long[] nums = new long[] { 3, 1, 4 };
        Console.WriteLine(Enumerable.Min(nums));
        Console.WriteLine(Enumerable.Max(nums));
    }
}
