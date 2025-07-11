using System;
using System.Collections.Generic;
using System.Linq;

class Program {
    static void Main() {
        var customers = new List<dynamic> { new Dictionary<dynamic, dynamic> { { "id", 1 }, { "name", "Alice" } }, new Dictionary<dynamic, dynamic> { { "id", 2 }, { "name", "Bob" } }, new Dictionary<dynamic, dynamic> { { "id", 3 }, { "name", "Charlie" } } };
        var orders = new List<dynamic> { new Dictionary<dynamic, dynamic> { { "id", 100 }, { "customerId", 1 } }, new Dictionary<dynamic, dynamic> { { "id", 101 }, { "customerId", 1 } }, new Dictionary<dynamic, dynamic> { { "id", 102 }, { "customerId", 2 } } };
        var stats = new Func<List<dynamic>>(() => {
    var groups = new Dictionary<string, _Group>();
    var order = new List<string>();
    foreach (var c in customers) {
        foreach (var o in orders) {
            if (!((o.customerId == c.id))) continue;
            var key = c.name;
            var ks = Convert.ToString(key);
            if (!groups.TryGetValue(ks, out var g)) {
                g = new _Group(key);
                groups[ks] = g;
                order.Add(ks);
            }
            g.Items.Add(c);
        }
    }
    var items = new List<_Group>();
    foreach (var ks in order) items.Add(groups[ks]);
    var _res = new List<dynamic>();
    foreach (var g in items) {
        _res.Add(new Dictionary<dynamic, dynamic> { { "name", g.key }, { "count", Enumerable.Count(g.Where(r => r["o"]).Select(r => r).ToArray()) } });
    }
    return _res;
})();
        Console.WriteLine("--- Group Left Join ---");
        foreach (var s in stats) {
            Console.WriteLine(string.Join(" ", new [] { Convert.ToString(s.name), Convert.ToString("orders:"), Convert.ToString(s.count) }));
        }
    }
    public class _Group {
        public dynamic key;
        public List<dynamic> Items = new List<dynamic>();
        public _Group(dynamic k) { key = k; }
    }
    
}
