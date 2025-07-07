using System;
using System.Collections.Generic;

class Program
{
    static void Main()
    {
        var a = new List<int> { 1, 2 };
        var result = new List<int>(a);
        result.Add(3);
        Console.WriteLine("[" + string.Join(", ", result) + "]");
    }
}
