using System;
using System.Collections.Generic;
using System.Linq;

class Program {
    static void Main() {
        var data = new List<dynamic> { new Dictionary<dynamic, dynamic> { { "a", 1 }, { "b", 2 } }, new Dictionary<dynamic, dynamic> { { "a", 1 }, { "b", 1 } }, new Dictionary<dynamic, dynamic> { { "a", 0 }, { "b", 5 } } };
        var sorted = data.OrderBy(x => new Dictionary<dynamic, dynamic> { { "a", x.a }, { "b", x.b } }).Select(x => x).ToArray();
        Console.WriteLine(JsonSerializer.Serialize(sorted));
    }
}
