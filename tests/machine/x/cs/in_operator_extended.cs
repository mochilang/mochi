using System;
using System.Collections.Generic;
using System.Linq;

class Program {
    static void Main() {
        List<int> xs = new List<int> { 1, 2, 3 };
        List<int> ys = xs.Where(x => ((x % 2) == 1)).Select(x => x).ToArray();
        Console.WriteLine(ys.Contains(1));
        Console.WriteLine(ys.Contains(2));
        var m = new Dictionary<dynamic, dynamic> { { "a", 1 } };
        Console.WriteLine(_in("a", m));
        Console.WriteLine(_in("b", m));
        string s = "hello";
        Console.WriteLine(s.Contains("ell"));
        Console.WriteLine(s.Contains("foo"));
    }
    static bool _in(dynamic item, dynamic col) {
        if (col is string s && item is string sub) {
            return s.Contains(sub);
        }
        if (col is System.Collections.IDictionary d) {
            return d.Contains(item);
        }
        if (col is System.Collections.IEnumerable e) {
            foreach (var it in e) {
                if (Equals(it, item)) return true;
            }
            return false;
        }
        return false;
    }
    
}
