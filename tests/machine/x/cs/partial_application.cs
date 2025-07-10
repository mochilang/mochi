using System;

class Program
{
    static long add(long a, long b)
    {
        return (a + b);
    }

    static void Main()
    {
        long add5 = new Func<long, long>((long p0) => { return add(5, p0); });
        Console.WriteLine(add5(3));
    }
}
