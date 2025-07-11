using System;
using System.Collections.Generic;
using System.Linq;

class Program {
    static void Main() {
        List<Nation> nations = new List<Nation> { new Nation { id = 1, name = "A" }, new Nation { id = 2, name = "B" } };
        List<Supplier> suppliers = new List<Supplier> { new Supplier { id = 1, nation = 1 }, new Supplier { id = 2, nation = 2 } };
        List<Partsupp> partsupp = new List<Partsupp> { new Partsupp { part = 100, supplier = 1, cost = 10.000000, qty = 2 }, new Partsupp { part = 100, supplier = 2, cost = 20.000000, qty = 1 }, new Partsupp { part = 200, supplier = 1, cost = 5.000000, qty = 3 } };
        List<Filtered> filtered = new Func<List<Filtered>>(() => {
    var _res = new List<Filtered>();
    foreach (var ps in partsupp) {
        foreach (var s in suppliers) {
            if (!((s.id == ps.supplier))) continue;
            foreach (var n in nations) {
                if (!((n.id == s.nation))) continue;
                if (!((n["name"] == "A"))) continue;
                _res.Add(new Filtered { part = ps.part, value = (ps.cost * ps.qty) });
            }
        }
    }
    return _res;
})();
        List<Grouped> grouped = _group_by(filtered, x => x.part).Select(g => new Grouped { part = g.Key, total = _sum(g.Items.Select(r => r.value).ToArray()) }).ToList();
        Console.WriteLine(JsonSerializer.Serialize(grouped));
    }
    public class Nation {
        public int id;
        public string name;
    }
    
    
    public class Supplier {
        public int id;
        public int nation;
    }
    
    
    public class Partsupp {
        public int part;
        public int supplier;
        public double cost;
        public int qty;
    }
    
    
    public class Filtered {
        public int part;
        public double value;
    }
    
    
    
    public class Grouped {
        public int part;
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
