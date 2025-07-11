using System;
using System.Collections.Generic;
using System.Linq;

class Program {
    static void Main() {
        Dictionary<string, int> m = new Dictionary<string, int> { { "a", 1 }, { "b", 2 }, { "c", 3 } };
        Console.WriteLine("[" + string.Join(", ", m.Values.ToList()) + "]");
    }
}
