using System;
using System.Collections.Generic;
using System.Linq;

class Entry { public int a; public int b; }

class Program {
    static void Main() {
        var data = new List<Entry> {
            new Entry{ a=1, b=2 },
            new Entry{ a=1, b=1 },
            new Entry{ a=0, b=5 }
        };
        var sorted = data.OrderBy(x => x.a).ThenBy(x => x.b).ToList();
        Console.WriteLine("[" + string.Join(", ", sorted.Select(x => $"{\"a\":{x.a}, \"b\":{x.b}}")) + "]");
    }
}
