// Generated by Mochi 0.10.34 on 2025-07-22 09:31 +0700
using System;
class Program {
    static bool boom(int a, int b) {
        Console.WriteLine("boom");
        return true;
    }
    static void Main() {
        Console.WriteLine(((false && boom(1, 2)) ? 1 : 0));
        Console.WriteLine(((true || boom(1, 2)) ? 1 : 0));
    }
}
