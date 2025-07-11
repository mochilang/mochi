using System;
using System.Collections.Generic;
using System.Linq;

class Program {
    static void Main() {
        List<int> xs = new List<int> { 1, 2, 3 };
        Console.WriteLine(xs.Contains(2));
        Console.WriteLine((!(xs.Contains(5))));
    }
}
