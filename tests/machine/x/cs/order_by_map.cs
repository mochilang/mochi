using System;
using System.Collections.Generic;
using System.Linq;

class Program {
    static void Main() {
        var data = new dynamic[] { new Dictionary<string, long> { { "a", 1 }, { "b", 2 } }, new Dictionary<string, long> { { "a", 1 }, { "b", 1 } }, new Dictionary<string, long> { { "a", 0 }, { "b", 5 } } };
        Dictionary<string, long>[] sorted = data.OrderBy(x => new Dictionary<string, long> { { "a", x["a"] }, { "b", x["b"] } }).Select(x => x).ToArray();
        Console.WriteLine(string.Join(" ", sorted));
    }
}
