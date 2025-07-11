using System;
using System.Collections.Generic;
using System.Linq;

class Program {
    static void Main() {
        var data = new List<dynamic> { new Dictionary<dynamic, dynamic> { { "tag", "a" }, { "val", 1 } }, new Dictionary<dynamic, dynamic> { { "tag", "a" }, { "val", 2 } }, new Dictionary<dynamic, dynamic> { { "tag", "b" }, { "val", 3 } } };
        var groups = _group_by(data, d => d.tag).Select(g => g).ToList();
        var tmp = new List<dynamic>();
        foreach (var g in groups) {
            int total = 0;
            foreach (var x in g.Items) {
                total = (total + x.val);
            }
            tmp = new List<dynamic>(tmp){new Dictionary<dynamic, dynamic> { { "tag", g.Key }, { "total", total } }};
        }
        var result = tmp.OrderBy(r => r["tag"]).Select(r => r).ToArray();
        Console.WriteLine(JsonSerializer.Serialize(result));
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
