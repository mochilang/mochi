using System;
using System.Collections.Generic;

class Program
{
    static void Main()
    {
        List<long> a = new List<long> { 1, 2 };
        Console.WriteLine(JsonSerializer.Serialize(new List<long>(a) { 3 }));
    }
}
