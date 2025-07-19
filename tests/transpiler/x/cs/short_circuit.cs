// Mochi 0.10.31 - generated 2025-07-19 10:19:08 UTC
using System;

class Program {
    static int boom(int a, int b) {
    Console.WriteLine("boom");
    return 1;
}
    static void Main() {
        Console.WriteLine(((0 != 0 && boom(1, 2) != 0) ? 1 : 0));
        Console.WriteLine(((1 != 0 || boom(1, 2) != 0) ? 1 : 0));
    }
}
