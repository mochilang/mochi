using System;
using System.Linq;

class Program
{
    static void Main()
    {
        int[] xs = { 1, 2, 3 };
        Console.WriteLine(xs.Contains(2));
        Console.WriteLine(!xs.Contains(5));
    }
}
