using System;

class Program
{
    static int Add(int a, int b) => a + b;

    static void Main()
    {
        Func<int, int> add5 = b => Add(5, b);
        Console.WriteLine(add5(3));
    }
}
