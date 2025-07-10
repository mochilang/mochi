using System;
using System.Collections.Generic;

class Program
{
    static void Main()
    {
        List<long> numbers = new List<long> { 1, 2, 3, 4, 5, 6, 7, 8, 9 };
        foreach (var n in numbers)
        {
            if ((n % 2) == 0)
            {
                continue;
            }
            if (n > 7)
            {
                break;
            }
            Console.WriteLine(string.Join(" ", new[] { Convert.ToString("odd number:"), Convert.ToString(n) }));
        }
    }
}
