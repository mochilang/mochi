using System;
using System.Collections.Generic;

class Program {
    static void Main() {
        Dictionary<string, int> scores = new Dictionary<string, int> { { "alice", 1 } };
        scores["bob"] = 2;
        Console.WriteLine(scores["bob"]);
    }
}
