using System;
using System.Collections.Generic;

class Program {
    static void Main() {
        Dictionary<string, int> m = new Dictionary<string, int> { { "a", 1 }, { "b", 2 } };
        foreach (var k in m.Keys) {
            Console.WriteLine(k);
        }
    }
}
