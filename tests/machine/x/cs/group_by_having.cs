using System;
using System.Collections.Generic;
using System.Linq;

class Program {
    static void Main() {
        var people = new List<dynamic> { new Dictionary<dynamic, dynamic> { { "name", "Alice" }, { "city", "Paris" } }, new Dictionary<dynamic, dynamic> { { "name", "Bob" }, { "city", "Hanoi" } }, new Dictionary<dynamic, dynamic> { { "name", "Charlie" }, { "city", "Paris" } }, new Dictionary<dynamic, dynamic> { { "name", "Diana" }, { "city", "Hanoi" } }, new Dictionary<dynamic, dynamic> { { "name", "Eve" }, { "city", "Paris" } }, new Dictionary<dynamic, dynamic> { { "name", "Frank" }, { "city", "Hanoi" } }, new Dictionary<dynamic, dynamic> { { "name", "George" }, { "city", "Paris" } } };
        var big = _group_by(people, p => p.city).Where(g => (Enumerable.Count(g) >= 4)).Select(g => new Dictionary<dynamic, dynamic> { { "city", g.Key }, { "num", Enumerable.Count(g) } }).ToList();
        Console.WriteLine(JsonSerializer.Serialize(big));
    }
    static List<_Group> _group_by(IEnumerable<dynamic> src, Func<dynamic, dynamic> keyfn) {
        var groups = new Dictionary<string, _Group>();
        var order = new List<string>();
        foreach (var it in src) {
            var key = keyfn(it);
            var ks = Convert.ToString(key);
            if (!groups.TryGetValue(ks, out var g)) {
                g = new _Group(key);
                groups[ks] = g;
                order.Add(ks);
            }
            g.Items.Add(it);
        }
        var res = new List<_Group>();
        foreach (var k in order) res.Add(groups[k]);
        return res;
    }
    
}
