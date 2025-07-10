using System;
using System.Collections.Generic;
using System.Linq;

class Program
{
    static void Main()
    {
        var items = new List<dynamic> { new Dictionary<string, dynamic> { { "cat", "a" }, { "val", 10 }, { "flag", true } }, new Dictionary<string, dynamic> { { "cat", "a" }, { "val", 5 }, { "flag", false } }, new Dictionary<string, dynamic> { { "cat", "b" }, { "val", 20 }, { "flag", true } } };
        var result = _group_by(items, i => i.cat).OrderBy(g => g.Key).Select(g => new Dictionary<string, dynamic> { { "cat", g.Key }, { "share", (_sum(g.Items.Select(x => (x.flag ? x.val : 0)).ToArray()) / _sum(g.Items.Select(x => x.val).ToArray())) } }).ToList();
        Console.WriteLine(JsonSerializer.Serialize(result));
    }
    static double _sum(dynamic v)
    {
        if (v == null) return 0.0;
        double _sum = 0;
        foreach (var it in v)
        {
            _sum += Convert.ToDouble(it);
        }
        return _sum;
    }

    static List<_Group> _group_by(IEnumerable<dynamic> src, Func<dynamic, dynamic> keyfn)
    {
        var groups = new Dictionary<string, _Group>();
        var order = new List<string>();
        foreach (var it in src)
        {
            var key = keyfn(it);
            var ks = Convert.ToString(key);
            if (!groups.TryGetValue(ks, out var g))
            {
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
