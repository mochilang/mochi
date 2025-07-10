using System;
using System.Collections.Generic;
using System.Linq;

class Program {
    static void Main() {
        var customers = new dynamic[] { new Dictionary<string, dynamic> { { "id", 1 }, { "name", "Alice" } }, new Dictionary<string, dynamic> { { "id", 2 }, { "name", "Bob" } }, new Dictionary<string, dynamic> { { "id", 3 }, { "name", "Charlie" } } };
        var orders = new dynamic[] { new Dictionary<string, long> { { "id", 100 }, { "customerId", 1 }, { "total", 250 } }, new Dictionary<string, long> { { "id", 101 }, { "customerId", 2 }, { "total", 125 } }, new Dictionary<string, long> { { "id", 102 }, { "customerId", 1 }, { "total", 300 } }, new Dictionary<string, long> { { "id", 103 }, { "customerId", 4 }, { "total", 80 } } };
        var result = new Func<List<Dictionary<string, dynamic>>>(() => {
    var _res = new List<Dictionary<string, dynamic>>();
    foreach (var o in orders) {
        foreach (var c in customers) {
            if (!((o["customerId"] == c["id"]))) continue;
            _res.Add(new Dictionary<string, dynamic> { { "orderId", o["id"] }, { "customerName", c["name"] }, { "total", o["total"] } });
        }
    }
    return _res;
})();
        Console.WriteLine("--- Orders with customer info ---");
        foreach (var entry in result) {
            Console.WriteLine(string.Join(" ", new [] { Convert.ToString("Order"), Convert.ToString(entry["orderId"]), Convert.ToString("by"), Convert.ToString(entry["customerName"]), Convert.ToString("- $"), Convert.ToString(entry["total"]) }));
        }
    }
}
