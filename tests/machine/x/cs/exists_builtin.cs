using System;
using System.Collections.Generic;
using System.Linq;

class Program
{
    static void Main()
    {
        List<long> data = new List<long> { 1, 2 };
        bool flag = Enumerable.Any(data.Where(x => (x == 1)).Select(x => x).ToArray());
        Console.WriteLine(flag);
    }
}
