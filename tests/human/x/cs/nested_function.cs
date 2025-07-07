using System;

class Program {
    static int Outer(int x) {
        int Inner(int y) => x + y;
        return Inner(5);
    }

    static void Main() {
        Console.WriteLine(Outer(3));
    }
}
