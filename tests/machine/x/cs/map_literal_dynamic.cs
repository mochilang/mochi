using System;
using System.Collections.Generic;

class Program {
    static void Main() {
        int x = 3;
        int y = 4;
        Dictionary<string, int> m = new Dictionary<string, int> { { "a", x }, { "b", y } };
        Console.WriteLine(string.Join(" ", new [] { Convert.ToString(m["a"]), Convert.ToString(m["b"]) }));
    }
}
