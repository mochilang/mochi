using System;
using System.Collections.Generic;

class Program {
    static void Main() {
        var m = new Dictionary<dynamic, dynamic> { { "a", 1 }, { "b", 2 } };
        Console.WriteLine(JsonSerializer.Serialize(m));
    }
}
