using System;
using System.Collections.Generic;
using System.Linq;

class Program {
    static void Main() {
        List<int> nums = new List<int> { 1, 2 };
        List<string> letters = new List<string> { "A", "B" };
        List<bool> bools = new List<bool> { true, false };
        List<Combo> combos = (
    from n in nums
    from l in letters
    from b in bools
    select new Combo { n = n, l = l, b = b }
).ToList();
        Console.WriteLine("--- Cross Join of three lists ---");
        foreach (var c in combos) {
            Console.WriteLine(string.Join(" ", new [] { Convert.ToString(c.n), Convert.ToString(c.l), Convert.ToString(c.b) }));
        }
    }
    public class Combo {
        public int n;
        public string l;
        public bool b;
    }
    
    
    
}
