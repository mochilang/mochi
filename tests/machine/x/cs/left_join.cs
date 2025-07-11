using System;
using System.Collections.Generic;
using System.Linq;

class Program {
    static void Main() {
        var customers = new List<dynamic> { new Dictionary<dynamic, dynamic> { { "id", 1 }, { "name", "Alice" } }, new Dictionary<dynamic, dynamic> { { "id", 2 }, { "name", "Bob" } } };
        var orders = new List<dynamic> { new Dictionary<dynamic, dynamic> { { "id", 100 }, { "customerId", 1 }, { "total", 250 } }, new Dictionary<dynamic, dynamic> { { "id", 101 }, { "customerId", 3 }, { "total", 80 } } };
        var result = new Func<List<dynamic>>(() => {
    var _res = new List<dynamic>();
    foreach (var o in orders) {
        bool _matched = false;
        foreach (var c in customers) {
            if (!((o.customerId == c.id))) continue;
            _matched = true;
            _res.Add(new Dictionary<dynamic, dynamic> { { "orderId", o.id }, { "customer", c }, { "total", o.total } });
        }
        if (!_matched) {
            dynamic c = default;
            _res.Add(new Dictionary<dynamic, dynamic> { { "orderId", o.id }, { "customer", c }, { "total", o.total } });
        }
    }
    return _res;
})();
        Console.WriteLine("--- Left Join ---");
        foreach (var entry in result) {
            Console.WriteLine(string.Join(" ", new [] { Convert.ToString("Order"), Convert.ToString(entry.orderId), Convert.ToString("customer"), Convert.ToString(entry.customer), Convert.ToString("total"), Convert.ToString(entry.total) }));
        }
    }
}
