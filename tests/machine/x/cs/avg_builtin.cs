using System;
using System.Linq;

class Program
{
    static void Main()
    {
        Console.WriteLine(Enumerable.Average(new long[] { 1, 2, 3 }.Select(_tmp0 => Convert.ToDouble(_tmp0))));
    }
}
