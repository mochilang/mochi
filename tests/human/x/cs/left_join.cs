using System;
using System.Collections.Generic;
using System.Linq;

class Customer { public int id; public string name; }
class Order { public int id; public int customerId; public int total; }

class Program {
    static void Main() {
        var customers = new List<Customer> {
            new Customer{ id=1, name="Alice" },
            new Customer{ id=2, name="Bob" }
        };
        var orders = new List<Order> {
            new Order{ id=100, customerId=1, total=250 },
            new Order{ id=101, customerId=3, total=80 }
        };

        var result = from o in orders
                     join c in customers on o.customerId equals c.id into temp
                     from c in temp.DefaultIfEmpty()
                     select new { orderId = o.id, customer = c, total = o.total };

        Console.WriteLine("--- Left Join ---");
        foreach (var entry in result) {
            Console.WriteLine($"Order {entry.orderId} customer {entry.customer?.name} total {entry.total}");
        }
    }
}
