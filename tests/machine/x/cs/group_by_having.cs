using System;
using System.Collections.Generic;
using System.Linq;

class Program {
    static void Main() {
        List<People> people = new List<People> { new People { name = "Alice", city = "Paris" }, new People { name = "Bob", city = "Hanoi" }, new People { name = "Charlie", city = "Paris" }, new People { name = "Diana", city = "Hanoi" }, new People { name = "Eve", city = "Paris" }, new People { name = "Frank", city = "Hanoi" }, new People { name = "George", city = "Paris" } };
        List<Big> big = _group_by<People, string>(people, p => p.city).Where(g => (Enumerable.Count(g) >= 4)).Select(g => new Big { city = g.Key, num = Enumerable.Count(g) }).ToList();
        Console.WriteLine(JsonSerializer.Serialize(big));
    }
    public class People {
        public string name;
        public string city;
    }
    
    
    public class Big {
        public string city;
        public int num;
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
