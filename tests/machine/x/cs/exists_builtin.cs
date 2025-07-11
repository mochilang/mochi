using System;
using System.Collections.Generic;
using System.Linq;

class Program
{
    static void Main()
    {
        List<int> data = new List<int> { 1, 2 };
        bool flag = Enumerable.Any(data.Where(x => (x == 1)).Select(x => x).ToArray());
        Console.WriteLine(flag);
    }
}
