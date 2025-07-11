using System;
using System.Collections.Generic;
using System.Linq;

class Program {
    static void Main() {
        var items = new List<dynamic> { new Dictionary<dynamic, dynamic> { { "cat", "a" }, { "val", 3 } }, new Dictionary<dynamic, dynamic> { { "cat", "a" }, { "val", 1 } }, new Dictionary<dynamic, dynamic> { { "cat", "b" }, { "val", 5 } }, new Dictionary<dynamic, dynamic> { { "cat", "b" }, { "val", 2 } } };
        var grouped = _group_by(items, i => i.cat).OrderBy(g => (-_sum(g.Items.Select(x => x.val).ToArray()))).Select(g => new Dictionary<dynamic, dynamic> { { "cat", g.Key }, { "total", _sum(g.Items.Select(x => x.val).ToArray()) } }).ToList();
        Console.WriteLine(JsonSerializer.Serialize(grouped));
    }
    static double _sum(dynamic v) {
        if (v == null) return 0.0;
        double _sum = 0;
        foreach (var it in v) {
            _sum += Convert.ToDouble(it);
        }
        return _sum;
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
