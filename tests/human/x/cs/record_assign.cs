using System;

class Counter
{
    public int n;
}

class Program
{
    static void Inc(Counter c)
    {
        c.n = c.n + 1;
    }

    static void Main()
    {
        var c = new Counter { n = 0 };
        Inc(c);
        Console.WriteLine(c.n);
    }
}
