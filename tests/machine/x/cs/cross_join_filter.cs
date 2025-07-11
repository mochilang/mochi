using System;
using System.Collections.Generic;
using System.Linq;

class Program {
    static void Main() {
        List<int> nums = new List<int> { 1, 2, 3 };
        List<string> letters = new List<string> { "A", "B" };
        List<Pair> pairs = (
    from n in nums
    from l in letters
    where ((n % 2) == 0)
    select new Pair { n = n, l = l }
).ToList();
        Console.WriteLine("--- Even pairs ---");
        foreach (var p in pairs) {
            Console.WriteLine(string.Join(" ", new [] { Convert.ToString(p.n), Convert.ToString(p.l) }));
        }
    }
    public class Pair {
        public int n;
        public string l;
    }
    
    
    
}
