using System;
using System.Collections.Generic;
using System.Linq;

class Program {
    static void Main() {
        List<Customer> customers = new List<Customer> { new Customer { id = 1, name = "Alice" }, new Customer { id = 2, name = "Bob" }, new Customer { id = 3, name = "Charlie" }, new Customer { id = 4, name = "Diana" } };
        List<Order> orders = new List<Order> { new Order { id = 100, customerId = 1, total = 250 }, new Order { id = 101, customerId = 2, total = 125 }, new Order { id = 102, customerId = 1, total = 300 }, new Order { id = 103, customerId = 5, total = 80 } };
        List<Result> result = new Func<List<Result>>(() => {
    var _res = new List<Result>();
    foreach (var o in orders) {
        var _joinItems = new List<Customer>(customers);
        var _matched = new bool[_joinItems.Count];
        foreach (var o in orders) {
            bool _m = false;
            for (int i = 0; i < _joinItems.Count; i++) {
                var c = _joinItems[i];
                if (!((o.customerId == c.id))) continue;
                _m = true;
                _matched[i] = true;
                _res.Add(new Result { order = o, customer = c });
            }
            if (!_m) {
                Customer c = default;
                _res.Add(new Result { order = o, customer = c });
            }
        }
        for (int i = 0; i < _joinItems.Count; i++) {
            if (!_matched[i]) {
                Order o = default;
                var c = _joinItems[i];
                _res.Add(new Result { order = o, customer = c });
            }
        }
    }
    return _res;
})();
        Console.WriteLine("--- Outer Join using syntax ---");
        foreach (var row in result) {
            if (row.order) {
                if (row.customer) {
                    Console.WriteLine(string.Join(" ", new [] { Convert.ToString("Order"), Convert.ToString(row.order.id), Convert.ToString("by"), Convert.ToString(row.customer.name), Convert.ToString("- $"), Convert.ToString(row.order.total) }));
                } else {
                    Console.WriteLine(string.Join(" ", new [] { Convert.ToString("Order"), Convert.ToString(row.order.id), Convert.ToString("by"), Convert.ToString("Unknown"), Convert.ToString("- $"), Convert.ToString(row.order.total) }));
                }
            } else {
                Console.WriteLine(string.Join(" ", new [] { Convert.ToString("Customer"), Convert.ToString(row.customer.name), Convert.ToString("has no orders") }));
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
        public Order order;
        public Customer customer;
    }
    
    
    
}
