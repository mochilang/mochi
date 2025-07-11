using System;
using System.Collections.Generic;
using System.Linq;

class Program {
    static void Main() {
        Console.WriteLine(Enumerable.Sum(new List<int> { 1, 2, 3 }.Select(_tmp0=>Convert.ToDouble(_tmp0))));
    }
}
