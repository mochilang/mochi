// Mochi 0.10.31 - generated 2025-07-19 12:59:56 UTC
using System;
using System.Collections.Generic;

class Program {
    static void Main() {
        var x = 3;
        var y = 4;
        var m = new Dictionary<object, object>{{"a", x}, {"b", y}};
        Console.WriteLine(m["a"], m["b"]);
    }
}
