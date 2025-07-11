using System;
using System.Collections.Generic;
using System.Linq;

class Program {
    static void Main() {
        List<int> a = new List<int> { 1, 2 };
        Console.WriteLine("[" + string.Join(", ", new List<int>(a){3}) + "]");
    }
}
