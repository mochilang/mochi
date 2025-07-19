// Mochi 0.10.31 - generated 2025-07-19 12:59:58 UTC
using System;
using System.Collections.Generic;

class Program {
    static void Main() {
        var m = new Dictionary<string, int>{{"a", 1}, {"b", 2}};
        Console.WriteLine((m.ContainsKey("a") ? 1 : 0));
        Console.WriteLine((m.ContainsKey("c") ? 1 : 0));
    }
}
