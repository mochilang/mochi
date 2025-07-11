using System;
using System.Collections.Generic;
using System.Linq;

class Program {
    static void Main() {
        List<Nation> nation = new List<Nation> { new Nation { n_nationkey = 1, n_name = "BRAZIL" } };
        List<Customer> customer = new List<Customer> { new Customer { c_custkey = 1, c_name = "Alice", c_acctbal = 100.000000, c_nationkey = 1, c_address = "123 St", c_phone = "123-456", c_comment = "Loyal" } };
        List<Order> orders = new List<Order> { new Order { o_orderkey = 1000, o_custkey = 1, o_orderdate = "1993-10-15" }, new Order { o_orderkey = 2000, o_custkey = 1, o_orderdate = "1994-01-02" } };
        List<Lineitem> lineitem = new List<Lineitem> { new Lineitem { l_orderkey = 1000, l_returnflag = "R", l_extendedprice = 1000.000000, l_discount = 0.100000 }, new Lineitem { l_orderkey = 2000, l_returnflag = "N", l_extendedprice = 500.000000, l_discount = 0.000000 } };
        string start_date = "1993-10-01";
        string end_date = "1994-01-01";
        List<Result> result = new Func<List<Result>>(() => {
    var groups = new Dictionary<string, _Group<Key, Customer>>();
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
                        var key = new Result { c_custkey = c.c_custkey, c_name = c.c_name, c_acctbal = c.c_acctbal, c_address = c.c_address, c_phone = c.c_phone, c_comment = c.c_comment, n_name = n.n_name };
                        var ks = Convert.ToString(key);
                        if (!groups.TryGetValue(ks, out var g)) {
                            g = new _Group<Key, Customer>(key);
                            groups[ks] = g;
                            order.Add(ks);
                        }
                        g.Items.Add(c);
                    }
                }
            }
        }
    }
    var items = new List<_Group<Key, Customer>>();
    foreach (var ks in order) items.Add(groups[ks]);
    items = items.OrderBy(g => (-_sum(g.Select(x => (x["l"]["l_extendedprice"] * ((1 - x["l"]["l_discount"])))).ToArray()))).ToList();
    var _res = new List<Result>();
    foreach (var g in items) {
        _res.Add(new Result { c_custkey = g.key.c_custkey, c_name = g.key.c_name, revenue = _sum(g.Select(x => (x["l"]["l_extendedprice"] * ((1 - x["l"]["l_discount"])))).ToArray()), c_acctbal = g.key.c_acctbal, n_name = g.key.n_name, c_address = g.key.c_address, c_phone = g.key.c_phone, c_comment = g.key.c_comment });
    }
    return _res;
})();
        Console.WriteLine(JsonSerializer.Serialize(result));
    }
    public class Nation {
        public int n_nationkey;
        public string n_name;
    }
    
    
    public class Customer {
        public int c_custkey;
        public string c_name;
        public double c_acctbal;
        public int c_nationkey;
        public string c_address;
        public string c_phone;
        public string c_comment;
    }
    
    
    public class Order {
        public int o_orderkey;
        public int o_custkey;
        public string o_orderdate;
    }
    
    
    public class Lineitem {
        public int l_orderkey;
        public string l_returnflag;
        public double l_extendedprice;
        public double l_discount;
    }
    
    
    public class Result {
        public int c_custkey;
        public string c_name;
        public int revenue;
        public double c_acctbal;
        public string n_name;
        public string c_address;
        public string c_phone;
        public string c_comment;
    }
    
    
    
    public class Key {
        public int c_custkey;
        public string c_name;
        public double c_acctbal;
        public string c_address;
        public string c_phone;
        public string c_comment;
        public string n_name;
    }
    
    
    static double _sum(dynamic v) {
        if (v == null) return 0.0;
        double _sum = 0;
        foreach (var it in v) {
            _sum += Convert.ToDouble(it);
        }
        return _sum;
    }
    
    public interface _IGroup { System.Collections.IEnumerable Items { get; } }
    public class _Group<TKey, TItem> : _IGroup {
        public TKey key;
        public List<TItem> Items = new List<TItem>();
        public _Group(TKey k) { key = k; }
        System.Collections.IEnumerable _IGroup.Items => Items;
    }
    
}
