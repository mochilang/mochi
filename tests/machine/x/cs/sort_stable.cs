using System;
using System.Collections.Generic;
using System.Linq;

class Program {
    static void Main() {
        var items = new List<dynamic> { new Dictionary<dynamic, dynamic> { { "n", 1 }, { "v", "a" } }, new Dictionary<dynamic, dynamic> { { "n", 1 }, { "v", "b" } }, new Dictionary<dynamic, dynamic> { { "n", 2 }, { "v", "c" } } };
        List<string> result = items.OrderBy(i => i.n).Select(i => i.v).ToArray();
        Console.WriteLine("[" + string.Join(", ", result) + "]");
    }
}
