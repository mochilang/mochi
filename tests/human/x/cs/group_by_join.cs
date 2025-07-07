using System;
using System.Collections.Generic;
using System.Linq;

class Customer { public int id; public string name; }
class Order { public int id; public int customerId; }

class Program {
    static void Main() {
        var customers = new List<Customer> {
            new Customer{ id=1, name="Alice" },
            new Customer{ id=2, name="Bob" }
        };
        var orders = new List<Order> {
            new Order{ id=100, customerId=1 },
            new Order{ id=101, customerId=1 },
            new Order{ id=102, customerId=2 }
        };

        var stats = from o in orders
                    join c in customers on o.customerId equals c.id
                    group o by c.name into g
                    select new { name = g.Key, count = g.Count() };

        Console.WriteLine("--- Orders per customer ---");
        foreach (var s in stats) {
            Console.WriteLine($"{s.name} orders: {s.count}");
        }
    }
}
