using System;
using System.Collections.Generic;

class Program {
    static void Main() {
        Dictionary<string, long> scores = new Dictionary<string, long> { { "alice", 1 } };
        scores["bob"] = 2;
        Console.WriteLine(scores["bob"]);
    }
}
