using System;
using System.Collections.Generic;
using System.Linq;

class Program {
    static void Main() {
        List<Customer> customers = new List<Customer> { new Customer { id = 1, name = "Alice" }, new Customer { id = 2, name = "Bob" }, new Customer { id = 3, name = "Charlie" }, new Customer { id = 4, name = "Diana" } };
        List<Order> orders = new List<Order> { new Order { id = 100, customerId = 1, total = 250 }, new Order { id = 101, customerId = 2, total = 125 }, new Order { id = 102, customerId = 1, total = 300 } };
        List<Result> result = new Func<List<Result>>(() => {
    var _res = new List<Result>();
    foreach (var c in customers) {
        bool _matched = false;
        foreach (var o in orders) {
            foreach (var c in customers) {
                if (!((o.customerId == c.id))) continue;
                _matched = true;
                _res.Add(new Result { customerName = c.name, order = o });
            }
            if (!_matched) {
                Customer c = default;
                _res.Add(new Result { customerName = c.name, order = o });
            }
        }
    }
    return _res;
})();
        Console.WriteLine("--- Right Join using syntax ---");
        foreach (var entry in result) {
            if (entry.order) {
                Console.WriteLine(string.Join(" ", new [] { Convert.ToString("Customer"), Convert.ToString(entry.customerName), Convert.ToString("has order"), Convert.ToString(entry.order.id), Convert.ToString("- $"), Convert.ToString(entry.order.total) }));
            } else {
                Console.WriteLine(string.Join(" ", new [] { Convert.ToString("Customer"), Convert.ToString(entry.customerName), Convert.ToString("has no orders") }));
            }
        }
    }
    public class Customer {
        public int id;
        public string name;
    }
    
    
    public class Order {
        public int id;
        public int customerId;
        public int total;
    }
    
    
    public class Result {
        public string customerName;
        public Order order;
    }
    
    
    
}
