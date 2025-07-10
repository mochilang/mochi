using System;
using System.Collections.Generic;
using System.Linq;

class Program {
    static void Main() {
        var customers = new dynamic[] { new Dictionary<string, dynamic> { { "id", 1 }, { "name", "Alice" } }, new Dictionary<string, dynamic> { { "id", 2 }, { "name", "Bob" } }, new Dictionary<string, dynamic> { { "id", 3 }, { "name", "Charlie" } }, new Dictionary<string, dynamic> { { "id", 4 }, { "name", "Diana" } } };
        var orders = new dynamic[] { new Dictionary<string, long> { { "id", 100 }, { "customerId", 1 }, { "total", 250 } }, new Dictionary<string, long> { { "id", 101 }, { "customerId", 2 }, { "total", 125 } }, new Dictionary<string, long> { { "id", 102 }, { "customerId", 1 }, { "total", 300 } } };
        var result = new Func<List<Dictionary<string, dynamic>>>(() => {
    var _res = new List<Dictionary<string, dynamic>>();
    foreach (var c in customers) {
        bool _matched = false;
        foreach (var o in orders) {
            foreach (var c in customers) {
                if (!((o["customerId"] == c["id"]))) continue;
                _matched = true;
                _res.Add(new Dictionary<string, dynamic> { { "customerName", c["name"] }, { "order", o } });
            }
            if (!_matched) {
                Dictionary<string, dynamic> c = default;
                _res.Add(new Dictionary<string, dynamic> { { "customerName", c["name"] }, { "order", o } });
            }
        }
    }
    return _res;
})();
        Console.WriteLine("--- Right Join using syntax ---");
        foreach (var entry in result) {
            if (entry["order"]) {
                Console.WriteLine(string.Join(" ", new [] { Convert.ToString("Customer"), Convert.ToString(entry["customerName"]), Convert.ToString("has order"), Convert.ToString(entry["order"].id), Convert.ToString("- $"), Convert.ToString(entry["order"].total) }));
            } else {
                Console.WriteLine(string.Join(" ", new [] { Convert.ToString("Customer"), Convert.ToString(entry["customerName"]), Convert.ToString("has no orders") }));
            }
        }
    }
}
