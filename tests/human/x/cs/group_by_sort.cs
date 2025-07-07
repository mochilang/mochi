using System;
using System.Collections.Generic;
using System.Linq;

class Item { public string cat; public int val; }

class Program {
    static void Main() {
        var items = new List<Item> {
            new Item{ cat="a", val=3 },
            new Item{ cat="a", val=1 },
            new Item{ cat="b", val=5 },
            new Item{ cat="b", val=2 }
        };

        var grouped = items
            .GroupBy(i => i.cat)
            .OrderByDescending(g => g.Sum(x => x.val))
            .Select(g => new { cat = g.Key, total = g.Sum(x => x.val) });

        foreach (var g in grouped) {
            Console.WriteLine($"{g.cat}: {g.total}");
        }
    }
}
