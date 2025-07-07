using System;
using System.Collections.Generic;
using System.Linq;

class Item { public int n; public string v; }

class Program {
    static void Main() {
        var items = new List<Item> {
            new Item{ n=1, v="a" },
            new Item{ n=1, v="b" },
            new Item{ n=2, v="c" }
        };
        var result = items.OrderBy(i => i.n).Select(i => i.v);
        Console.WriteLine("[" + string.Join(",", result) + "]");
    }
}
