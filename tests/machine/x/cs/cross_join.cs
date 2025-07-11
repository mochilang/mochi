using System;
using System.Collections.Generic;
using System.Linq;

class Program {
    static void Main() {
        List<Customer> customers = new List<Customer> { new Customer { id = 1, name = "Alice" }, new Customer { id = 2, name = "Bob" }, new Customer { id = 3, name = "Charlie" } };
        List<Order> orders = new List<Order> { new Order { id = 100, customerId = 1, total = 250 }, new Order { id = 101, customerId = 2, total = 125 }, new Order { id = 102, customerId = 1, total = 300 } };
        List<Result> result = new Func<List<Result>>(() => {
    var _res = new List<Result>();
    foreach (var o in orders) {
        foreach (var c in customers) {
            _res.Add(new Result { orderId = o.id, orderCustomerId = o.customerId, pairedCustomerName = c.name, orderTotal = o.total });
        }
    }
    return _res;
})();
        Console.WriteLine("--- Cross Join: All order-customer pairs ---");
        foreach (var entry in result) {
            Console.WriteLine(string.Join(" ", new [] { Convert.ToString("Order"), Convert.ToString(entry.orderId), Convert.ToString("(customerId:"), Convert.ToString(entry.orderCustomerId), Convert.ToString(", total: $"), Convert.ToString(entry.orderTotal), Convert.ToString(") paired with"), Convert.ToString(entry.pairedCustomerName) }));
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
        public int orderCustomerId;
        public string pairedCustomerName;
        public int orderTotal;
    }
    
    
    
}
