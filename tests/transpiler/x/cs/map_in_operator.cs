// Mochi 0.10.31 - generated 2025-07-19 13:00:01 UTC
using System;
using System.Collections.Generic;

class Program {
    static void Main() {
        var m = new Dictionary<int, string>{{1, "a"}, {2, "b"}};
        Console.WriteLine((m.ContainsKey(1) ? 1 : 0));
        Console.WriteLine((m.ContainsKey(3) ? 1 : 0));
    }
}
