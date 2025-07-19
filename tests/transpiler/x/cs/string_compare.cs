// Mochi 0.10.31 - generated 2025-07-19 12:00:43 UTC
using System;

class Program {
    static void Main() {
        Console.WriteLine((string.Compare("a", "b") < 0 ? 1 : 0));
        Console.WriteLine((string.Compare("a", "a") <= 0 ? 1 : 0));
        Console.WriteLine((string.Compare("b", "a") > 0 ? 1 : 0));
        Console.WriteLine((string.Compare("b", "b") >= 0 ? 1 : 0));
    }
}
