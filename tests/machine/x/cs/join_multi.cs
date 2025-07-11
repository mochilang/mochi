using System;
using System.Collections.Generic;
using System.Linq;

class Program {
    static void Main() {
        var customers = new List<dynamic> { new Dictionary<dynamic, dynamic> { { "id", 1 }, { "name", "Alice" } }, new Dictionary<dynamic, dynamic> { { "id", 2 }, { "name", "Bob" } } };
        var orders = new List<dynamic> { new Dictionary<dynamic, dynamic> { { "id", 100 }, { "customerId", 1 } }, new Dictionary<dynamic, dynamic> { { "id", 101 }, { "customerId", 2 } } };
        var items = new List<dynamic> { new Dictionary<dynamic, dynamic> { { "orderId", 100 }, { "sku", "a" } }, new Dictionary<dynamic, dynamic> { { "orderId", 101 }, { "sku", "b" } } };
        var result = new Func<List<dynamic>>(() => {
    var _res = new List<dynamic>();
    foreach (var o in orders) {
        foreach (var c in customers) {
            if (!((o.customerId == c.id))) continue;
            foreach (var i in items) {
                if (!((o.id == i.orderId))) continue;
                _res.Add(new Dictionary<dynamic, dynamic> { { "name", c.name }, { "sku", i.sku } });
            }
        }
    }
    return _res;
})();
        Console.WriteLine("--- Multi Join ---");
        foreach (var r in result) {
            Console.WriteLine(string.Join(" ", new [] { Convert.ToString(r.name), Convert.ToString("bought item"), Convert.ToString(r.sku) }));
        }
    }
}
