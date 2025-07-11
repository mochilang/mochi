using System;
using System.Collections.Generic;
using System.Linq;

class Program {
    static void Main() {
        var nation = new List<dynamic> { new Dictionary<dynamic, dynamic> { { "n_nationkey", 1 }, { "n_name", "BRAZIL" } } };
        var customer = new List<dynamic> { new Dictionary<dynamic, dynamic> { { "c_custkey", 1 }, { "c_name", "Alice" }, { "c_acctbal", 100.000000 }, { "c_nationkey", 1 }, { "c_address", "123 St" }, { "c_phone", "123-456" }, { "c_comment", "Loyal" } } };
        var orders = new List<dynamic> { new Dictionary<dynamic, dynamic> { { "o_orderkey", 1000 }, { "o_custkey", 1 }, { "o_orderdate", "1993-10-15" } }, new Dictionary<dynamic, dynamic> { { "o_orderkey", 2000 }, { "o_custkey", 1 }, { "o_orderdate", "1994-01-02" } } };
        var lineitem = new List<dynamic> { new Dictionary<dynamic, dynamic> { { "l_orderkey", 1000 }, { "l_returnflag", "R" }, { "l_extendedprice", 1000.000000 }, { "l_discount", 0.100000 } }, new Dictionary<dynamic, dynamic> { { "l_orderkey", 2000 }, { "l_returnflag", "N" }, { "l_extendedprice", 500.000000 }, { "l_discount", 0.000000 } } };
        string start_date = "1993-10-01";
        string end_date = "1994-01-01";
        var result = new Func<List<dynamic>>(() => {
    var groups = new Dictionary<string, _Group>();
    var order = new List<string>();
    foreach (var c in customer) {
        foreach (var o in orders) {
            if (!((o.o_custkey == c.c_custkey))) continue;
            foreach (var l in lineitem) {
                if (!((l.l_orderkey == o.o_orderkey))) continue;
                if (!((l["l_returnflag"] == "R"))) continue;
                foreach (var n in nation) {
                    if (!((n.n_nationkey == c.c_nationkey))) continue;
                    if ((o["o_orderdate"] >= start_date) && (o["o_orderdate"] < end_date)) {
                        var key = new Dictionary<dynamic, dynamic> { { "c_custkey", c.c_custkey }, { "c_name", c.c_name }, { "c_acctbal", c.c_acctbal }, { "c_address", c.c_address }, { "c_phone", c.c_phone }, { "c_comment", c.c_comment }, { "n_name", n.n_name } };
                        var ks = Convert.ToString(key);
                        if (!groups.TryGetValue(ks, out var g)) {
                            g = new _Group(key);
                            groups[ks] = g;
                            order.Add(ks);
                        }
                        g.Items.Add(c);
                    }
                }
            }
        }
    }
    var items = new List<_Group>();
    foreach (var ks in order) items.Add(groups[ks]);
    items = items.OrderBy(g => (-_sum(g.Select(x => (x["l"]["l_extendedprice"] * ((1 - x["l"]["l_discount"])))).ToArray()))).ToList();
    var _res = new List<dynamic>();
    foreach (var g in items) {
        _res.Add(new Dictionary<dynamic, dynamic> { { "c_custkey", g.key.c_custkey }, { "c_name", g.key.c_name }, { "revenue", _sum(g.Select(x => (x["l"]["l_extendedprice"] * ((1 - x["l"]["l_discount"])))).ToArray()) }, { "c_acctbal", g.key.c_acctbal }, { "n_name", g.key.n_name }, { "c_address", g.key.c_address }, { "c_phone", g.key.c_phone }, { "c_comment", g.key.c_comment } });
    }
    return _res;
})();
        Console.WriteLine(JsonSerializer.Serialize(result));
    }
    static double _sum(dynamic v) {
        if (v == null) return 0.0;
        double _sum = 0;
        foreach (var it in v) {
            _sum += Convert.ToDouble(it);
        }
        return _sum;
    }
    
    public class _Group {
        public dynamic key;
        public List<dynamic> Items = new List<dynamic>();
        public _Group(dynamic k) { key = k; }
    }
    
}
