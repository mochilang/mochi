using System;
using System.Collections.Generic;
using System.Linq;

class Program {
    static void Main() {
        var customers = new List<dynamic> { new Dictionary<dynamic, dynamic> { { "id", 1 }, { "name", "Alice" } }, new Dictionary<dynamic, dynamic> { { "id", 2 }, { "name", "Bob" } } };
        var orders = new List<dynamic> { new Dictionary<dynamic, dynamic> { { "id", 100 }, { "customerId", 1 } }, new Dictionary<dynamic, dynamic> { { "id", 101 }, { "customerId", 2 } } };
        var items = new List<dynamic> { new Dictionary<dynamic, dynamic> { { "orderId", 100 }, { "sku", "a" } } };
        var result = new Func<List<dynamic>>(() => {
    var _res = new List<dynamic>();
    foreach (var o in orders) {
        foreach (var c in customers) {
            if (!((o.customerId == c.id))) continue;
            foreach (var i in items) {
                if (!((o.id == i.orderId))) continue;
                _res.Add(new Dictionary<dynamic, dynamic> { { "orderId", o.id }, { "name", c.name }, { "item", i } });
            }
        }
    }
    return _res;
})();
        Console.WriteLine("--- Left Join Multi ---");
        foreach (var r in result) {
            Console.WriteLine(string.Join(" ", new [] { Convert.ToString(r.orderId), Convert.ToString(r.name), Convert.ToString(r.item) }));
        }
    }
}
