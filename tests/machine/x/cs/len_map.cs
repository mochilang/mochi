using System;
using System.Collections.Generic;

class Program {
    static void Main() {
        Console.WriteLine(new Dictionary<string, int> { { "a", 1 }, { "b", 2 } }.Count);
    }
}
