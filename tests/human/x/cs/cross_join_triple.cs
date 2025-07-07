using System;
using System.Linq;

class Program {
    static void Main() {
        var nums = new[]{1,2};
        var letters = new[]{"A","B"};
        var bools = new[]{true,false};
        var combos = from n in nums
                     from l in letters
                     from b in bools
                     select new {n,l,b};
        Console.WriteLine("--- Cross Join of three lists ---");
        foreach (var c in combos) {
            Console.WriteLine($"{c.n} {c.l} {c.b}");
        }
    }
}
