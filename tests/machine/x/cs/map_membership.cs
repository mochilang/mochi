using System;
using System.Collections.Generic;

class Program {
    static void Main() {
        Dictionary<string, int> m = new Dictionary<string, int> { { "a", 1 }, { "b", 2 } };
        Console.WriteLine(m.ContainsKey("a"));
        Console.WriteLine(m.ContainsKey("c"));
    }
}
