using System;
using System.Collections.Generic;
using System.Linq;

class Program {
    static void Main() {
        var customers = new List<dynamic> { new Dictionary<dynamic, dynamic> { { "id", 1 }, { "name", "Alice" } }, new Dictionary<dynamic, dynamic> { { "id", 2 }, { "name", "Bob" } }, new Dictionary<dynamic, dynamic> { { "id", 3 }, { "name", "Charlie" } } };
        var orders = new List<dynamic> { new Dictionary<dynamic, dynamic> { { "id", 100 }, { "customerId", 1 }, { "total", 250 } }, new Dictionary<dynamic, dynamic> { { "id", 101 }, { "customerId", 2 }, { "total", 125 } }, new Dictionary<dynamic, dynamic> { { "id", 102 }, { "customerId", 1 }, { "total", 300 } }, new Dictionary<dynamic, dynamic> { { "id", 103 }, { "customerId", 4 }, { "total", 80 } } };
        var result = orders.Join(customers, o => o.customerId, c => c.id, (o, c) => new Dictionary<dynamic, dynamic> { { "orderId", o.id }, { "customerName", c.name }, { "total", o.total } }).ToList();
        Console.WriteLine("--- Orders with customer info ---");
        foreach (var entry in result) {
            Console.WriteLine(string.Join(" ", new [] { Convert.ToString("Order"), Convert.ToString(entry.orderId), Convert.ToString("by"), Convert.ToString(entry.customerName), Convert.ToString("- $"), Convert.ToString(entry.total) }));
        }
    }
}
