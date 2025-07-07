using System;

class Program {
    static bool Boom() {
        Console.WriteLine("boom");
        return true;
    }

    static void Main() {
        Console.WriteLine((1 < 2) && (2 < 3) && (3 < 4));
        Console.WriteLine((1 < 2) && (2 > 3) && Boom());
        Console.WriteLine((1 < 2) && (2 < 3) && (3 > 4) && Boom());
    }
}
