using System;
using System.Linq;

class Program
{
    static void Main()
    {
        long[] data = new long[] { 1, 2 };
        bool flag = Enumerable.Any(data.Where(x => (x == 1)).Select(x => x).ToArray());
        Console.WriteLine(flag);
    }
}
