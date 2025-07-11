using System;
using System.Collections.Generic;
using System.Linq;

class Program {
    static void Main() {
        List<Item> items = new List<Item> { new Item { cat = "a", val = 3 }, new Item { cat = "a", val = 1 }, new Item { cat = "b", val = 5 }, new Item { cat = "b", val = 2 } };
        List<Grouped> grouped = _group_by<Item, string>(items, i => i.cat).OrderBy(g => (-_sum(g.Items.Select(x => x.val).ToArray()))).Select(g => new Grouped { cat = g.Key, total = _sum(g.Items.Select(x => x.val).ToArray()) }).ToList();
        Console.WriteLine(JsonSerializer.Serialize(grouped));
    }
    public class Item {
        public string cat;
        public int val;
    }
    
    
    public class Grouped {
        public string cat;
        public int total;
    }
    
    
    
    static double _sum(dynamic v) {
        if (v == null) return 0.0;
        double _sum = 0;
        foreach (var it in v) {
            _sum += Convert.ToDouble(it);
        }
        return _sum;
    }
    
    static List<_Group<TKey, TItem>> _group_by<TItem, TKey>(IEnumerable<TItem> src, Func<TItem, TKey> keyfn) {
        var groups = new Dictionary<string, _Group<TKey, TItem>>();
        var order = new List<string>();
        foreach (var it in src) {
            var key = keyfn(it);
            var ks = Convert.ToString(key);
            if (!groups.TryGetValue(ks, out var g)) {
                g = new _Group<TKey, TItem>(key);
                groups[ks] = g;
                order.Add(ks);
            }
            g.Items.Add(it);
        }
        var res = new List<_Group<TKey, TItem>>();
        foreach (var k in order) res.Add(groups[k]);
        return res;
    }
    
}
