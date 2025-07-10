using System;
using System.Collections.Generic;

class Program {
    static void Main() {
        Dictionary<string, long> m = new Dictionary<string, long> { { "a", 1 }, { "b", 2 } };
        Console.WriteLine(m["b"]);
    }
}
