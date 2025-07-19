// Mochi 0.10.31 - generated 2025-07-19 06:29:41 UTC
using System;

class Program {
    static void Main() {
        var x = 8;
        var msg = ((x > 10 ? 1 : 0) != 0 ? "big" : ((x > 5 ? 1 : 0) != 0 ? "medium" : "small"));
        Console.WriteLine(msg);
    }
}
