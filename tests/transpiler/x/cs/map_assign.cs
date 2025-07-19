// Mochi 0.10.31 - generated 2025-07-19 12:59:48 UTC
using System;
using System.Collections.Generic;

class Program {
    static void Main() {
        var scores = new Dictionary<object, object>{{"alice", 1}};
        scores["bob"] = 2;
        Console.WriteLine(scores["bob"]);
    }
}
