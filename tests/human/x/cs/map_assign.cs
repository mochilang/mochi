using System;
using System.Collections.Generic;

class Program
{
    static void Main()
    {
        var scores = new Dictionary<string, int> { { "alice", 1 } };
        scores["bob"] = 2;
        Console.WriteLine(scores["bob"]);
    }
}
