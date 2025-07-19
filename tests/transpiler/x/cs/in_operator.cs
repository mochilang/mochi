// Mochi 0.10.31 - generated 2025-07-19 10:19:08 UTC
using System;

class Program {
    static void Main() {
        var xs = new[]{1, 2, 3};
        Console.WriteLine((Array.IndexOf(xs, 2) >= 0 ? 1 : 0));
        Console.WriteLine(((Array.IndexOf(xs, 5) >= 0 ? 1 : 0) == 0 ? 1 : 0));
    }
}
