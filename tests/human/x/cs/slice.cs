using System;
using System.Linq;

class Program
{
    static void Main()
    {
        var arr = new[] { 1, 2, 3 };
        Console.WriteLine("[" + string.Join(",", arr.Skip(1).Take(2)) + "]");
        Console.WriteLine("[" + string.Join(",", arr.Take(2)) + "]");
        Console.WriteLine("hello".Substring(1, 3));
    }
}
