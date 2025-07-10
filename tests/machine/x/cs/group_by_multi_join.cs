using System;
using System.Collections.Generic;
using System.Linq;

class Program
{
    static void Main()
    {
        var nations = new dynamic[] { new Dictionary<string, dynamic> { { "id", 1 }, { "name", "A" } }, new Dictionary<string, dynamic> { { "id", 2 }, { "name", "B" } } };
        var suppliers = new dynamic[] { new Dictionary<string, long> { { "id", 1 }, { "nation", 1 } }, new Dictionary<string, long> { { "id", 2 }, { "nation", 2 } } };
        var partsupp = new dynamic[] { new Dictionary<string, dynamic> { { "part", 100 }, { "supplier", 1 }, { "cost", 10.000000 }, { "qty", 2 } }, new Dictionary<string, dynamic> { { "part", 100 }, { "supplier", 2 }, { "cost", 20.000000 }, { "qty", 1 } }, new Dictionary<string, dynamic> { { "part", 200 }, { "supplier", 1 }, { "cost", 5.000000 }, { "qty", 3 } } };
        var filtered = new Func<List<Dictionary<string, dynamic>>>(() =>
        {
            var _res = new List<Dictionary<string, dynamic>>();
            foreach (var ps in partsupp)
            {
                foreach (var s in suppliers)
                {
                    if (!((s.id == ps.supplier))) continue;
                    foreach (var n in nations)
                    {
                        if (!((n.id == s.nation))) continue;
                        if (!((n["name"] == "A"))) continue;
                        _res.Add(new Dictionary<string, dynamic> { { "part", ps.part }, { "value", (ps.cost * ps.qty) } });
                    }
                }
            }
            return _res;
        })();
        var grouped = _group_by(filtered, x => x["part"]).Select(g => new Dictionary<string, dynamic> { { "part", g.Key }, { "total", _sum(g.Items.Select(r => r["value"]).ToArray()) } }).ToList();
        Console.WriteLine(JsonSerializer.Serialize(grouped));
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
