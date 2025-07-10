using System;
using System.Collections.Generic;

class Program {
    static void Main() {
        long x = 3;
        long y = 4;
        Dictionary<string, long> m = new Dictionary<string, long> { { "a", x }, { "b", y } };
        Console.WriteLine(string.Join(" ", new [] { Convert.ToString(m["a"]), Convert.ToString(m["b"]) }));
    }
}
