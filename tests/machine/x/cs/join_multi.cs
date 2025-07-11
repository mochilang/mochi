using System;
using System.Collections.Generic;
using System.Linq;

class Program {
    static void Main() {
        List<Customer> customers = new List<Customer> { new Customer { id = 1, name = "Alice" }, new Customer { id = 2, name = "Bob" } };
        List<Order> orders = new List<Order> { new Order { id = 100, customerId = 1 }, new Order { id = 101, customerId = 2 } };
        List<Item> items = new List<Item> { new Item { orderId = 100, sku = "a" }, new Item { orderId = 101, sku = "b" } };
        List<Result> result = (
    from o in orders
    join c in customers on o.customerId equals c.id
    join i in items on o.id equals i.orderId
    select new Result { name = c["name"], sku = i["sku"] }
).ToList();
        Console.WriteLine("--- Multi Join ---");
        foreach (var r in result) {
            Console.WriteLine(string.Join(" ", new [] { Convert.ToString(r.name), Convert.ToString("bought item"), Convert.ToString(r.sku) }));
        }
    }
    public class Customer {
        public int id;
        public string name;
    }
    
    
    public class Order {
        public int id;
        public int customerId;
    }
    
    
    public class Item {
        public int orderId;
        public string sku;
    }
    
    
    public class Result {
        public string name;
        public string sku;
    }
    
    
    
}
