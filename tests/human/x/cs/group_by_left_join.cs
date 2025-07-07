using System;
using System.Collections.Generic;
using System.Linq;

class Customer { public int id; public string name; }
class Order { public int id; public int customerId; }

class Program {
    static void Main() {
        var customers = new List<Customer> {
            new Customer{ id=1, name="Alice" },
            new Customer{ id=2, name="Bob" },
            new Customer{ id=3, name="Charlie" }
        };
        var orders = new List<Order> {
            new Order{ id=100, customerId=1 },
            new Order{ id=101, customerId=1 },
            new Order{ id=102, customerId=2 }
        };

        var stats = from c in customers
                    join o in orders on c.id equals o.customerId into os
                    from o in os.DefaultIfEmpty()
                    group o by c.name into g
                    select new {
                        name = g.Key,
                        count = g.Count(x => x != null)
                    };

        Console.WriteLine("--- Group Left Join ---");
        foreach (var s in stats) {
            Console.WriteLine($"{s.name} orders: {s.count}");
        }
    }
}
