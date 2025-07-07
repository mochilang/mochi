using System;

class Program {
    static int k = 2;
    static int Inc(int x) => x + k;
    static void Main() {
        Console.WriteLine(Inc(3));
    }
}
