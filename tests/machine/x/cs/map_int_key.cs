using System;
using System.Collections.Generic;

class Program {
    static void Main() {
        Dictionary<long, string> m = new Dictionary<long, string> { { 1, "a" }, { 2, "b" } };
        Console.WriteLine(m[1]);
    }
}
