using System;
using System.Collections.Generic;
using System.Linq;

class Program {
    static void Main() {
        List<Customer> customers = new List<Customer> { new Customer { id = 1, name = "Alice" }, new Customer { id = 2, name = "Bob" }, new Customer { id = 3, name = "Charlie" } };
        List<Order> orders = new List<Order> { new Order { id = 100, customerId = 1 }, new Order { id = 101, customerId = 1 }, new Order { id = 102, customerId = 2 } };
        List<Stat> stats = new Func<List<Stat>>(() => {
    var groups = new Dictionary<string, _Group<string, Customer>>();
    var order = new List<string>();
    foreach (var c in customers) {
        foreach (var o in orders) {
            if (!((o.customerId == c.id))) continue;
            var key = c.name;
            var ks = Convert.ToString(key);
            if (!groups.TryGetValue(ks, out var g)) {
                g = new _Group<string, Customer>(key);
                groups[ks] = g;
                order.Add(ks);
            }
            g.Items.Add(c);
        }
    }
    var items = new List<_Group<string, Customer>>();
    foreach (var ks in order) items.Add(groups[ks]);
    var _res = new List<Stat>();
    foreach (var g in items) {
        _res.Add(new Stat { name = g.key, count = Enumerable.Count(g.Where(r => r["o"]).Select(r => r).ToArray()) });
    }
    return _res;
})();
        Console.WriteLine("--- Group Left Join ---");
        foreach (var s in stats) {
            Console.WriteLine(string.Join(" ", new [] { Convert.ToString(s.name), Convert.ToString("orders:"), Convert.ToString(s.count) }));
        }
    }
    public class Customer {
        public int id;
        public string name;
    }
    
    
    public class Order {
        public int id;
        public int customerId;
    }
    
    
    public class Stat {
        public string name;
        public int count;
    }
    
    
    
    public interface _IGroup { System.Collections.IEnumerable Items { get; } }
    public class _Group<TKey, TItem> : _IGroup {
        public TKey key;
        public List<TItem> Items = new List<TItem>();
        public _Group(TKey k) { key = k; }
        System.Collections.IEnumerable _IGroup.Items => Items;
    }
    
}
