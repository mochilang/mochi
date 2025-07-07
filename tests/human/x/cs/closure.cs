using System;

class Program
{
    static Func<int, int> MakeAdder(int n)
    {
        return (int x) => x + n;
    }

    static void Main()
    {
        var add10 = MakeAdder(10);
        Console.WriteLine(add10(7));
    }
}
