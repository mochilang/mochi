using System;
using System.Linq;

class Program
{
    static void Main()
    {
        int[] data = { 1, 2 };
        bool flag = data.Any(x => x == 1);
        Console.WriteLine(flag);
    }
}
