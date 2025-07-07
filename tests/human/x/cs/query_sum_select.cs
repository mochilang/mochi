using System;
using System.Linq;

class Program
{
    static void Main()
    {
        var nums = new[] { 1, 2, 3 };
        var result = nums.Where(n => n > 1).Sum(n => n);
        Console.WriteLine(result);
    }
}
