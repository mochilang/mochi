using System;
using System.Collections.Generic;
using System.Linq;

class Program {
    static void Main() {
        List<Customer> customers = new List<Customer> { new Customer { id = 1, name = "Alice" }, new Customer { id = 2, name = "Bob" }, new Customer { id = 3, name = "Charlie" } };
        List<Order> orders = new List<Order> { new Order { id = 100, customerId = 1, total = 250 }, new Order { id = 101, customerId = 2, total = 125 }, new Order { id = 102, customerId = 1, total = 300 }, new Order { id = 103, customerId = 4, total = 80 } };
        List<Result> result = orders.Join(customers, o => o.customerId, c => c.id, (o, c) => new Result { orderId = o.id, customerName = c.name, total = o.total }).ToList();
        Console.WriteLine("--- Orders with customer info ---");
        foreach (var entry in result) {
            Console.WriteLine(string.Join(" ", new [] { Convert.ToString("Order"), Convert.ToString(entry.orderId), Convert.ToString("by"), Convert.ToString(entry.customerName), Convert.ToString("- $"), Convert.ToString(entry.total) }));
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
        public string customerName;
        public int total;
    }
    
    
    
}
