using System;
using System.Collections.Generic;
using System.Linq;

class Program {
    static void Main() {
        List<Customer> customers = new List<Customer> { new Customer { id = 1, name = "Alice" }, new Customer { id = 2, name = "Bob" } };
        List<Order> orders = new List<Order> { new Order { id = 100, customerId = 1, total = 250 }, new Order { id = 101, customerId = 3, total = 80 } };
        List<Result> result = new Func<List<Result>>(() => {
    var _res = new List<Result>();
    foreach (var o in orders) {
        bool _matched = false;
        foreach (var c in customers) {
            if (!((o.customerId == c.id))) continue;
            _matched = true;
            _res.Add(new Result { orderId = o.id, customer = c, total = o.total });
        }
        if (!_matched) {
            Customer c = default;
            _res.Add(new Result { orderId = o.id, customer = c, total = o.total });
        }
    }
    return _res;
})();
        Console.WriteLine("--- Left Join ---");
        foreach (var entry in result) {
            Console.WriteLine(string.Join(" ", new [] { Convert.ToString("Order"), Convert.ToString(entry.orderId), Convert.ToString("customer"), Convert.ToString(entry.customer), Convert.ToString("total"), Convert.ToString(entry.total) }));
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
        public int orderId;
        public Customer customer;
        public int total;
    }
    
    
    
}
