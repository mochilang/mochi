using System;

class Program
{
    static Func<long, long> makeAdder(long n)
    {
        return new Func<long, long>((long x) =>
        {
            return (x + n);
        });
    }

    static void Main()
    {
        Func<long, long> add10 = makeAdder(10);
        Console.WriteLine(add10(7));
    }
}
