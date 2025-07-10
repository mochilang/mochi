using System;
using System.Linq;

class Program
{
    static void Main()
    {
        long[] xs = new long[] { 1, 2, 3 };
        Console.WriteLine(xs.Contains(2));
        Console.WriteLine((!(xs.Contains(5))));
    }
}
