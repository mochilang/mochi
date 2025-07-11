using System;

class Program
{
    static long sum3(long a, long b, long c)
    {
        return ((a + b) + c);
    }

    static void Main()
    {
        Console.WriteLine(sum3(1, 2, 3));
    }
}
