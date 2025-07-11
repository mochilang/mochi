using System;

class Program {
    static bool boom(int a, int b) {
        Console.WriteLine("boom");
        return true;
    }
    
    static void Main() {
        Console.WriteLine((false && boom(1, 2)));
        Console.WriteLine((true || boom(1, 2)));
    }
}
