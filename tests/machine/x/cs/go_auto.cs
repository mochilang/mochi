using System;

static class testpkg {
    public static int Add(int a, int b) { return a + b; }
    public const double Pi = 3.14;
    public const int Answer = 42;
}

class Program {
    static void Main() {
        Console.WriteLine(testpkg.Add(2, 3));
        Console.WriteLine(testpkg.Pi);
        Console.WriteLine(testpkg.Answer);
    }
}
