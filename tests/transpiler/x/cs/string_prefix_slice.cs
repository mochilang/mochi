// Mochi 0.10.31 - generated 2025-07-19 10:59:06 UTC
using System;

class Program {
    static void Main() {
        var prefix = "fore";
        var s1 = "forest";
        Console.WriteLine((s1.Substring(0, (prefix.Length - 0)) == prefix ? 1 : 0));
        var s2 = "desert";
        Console.WriteLine((s2.Substring(0, (prefix.Length - 0)) == prefix ? 1 : 0));
    }
}
