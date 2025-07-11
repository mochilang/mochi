using System;
using System.Collections.Generic;
using System.Linq;

class Program {
    static void Main() {
        List<Customer> customers = new List<Customer> { new Customer { id = 1, name = "Alice" }, new Customer { id = 2, name = "Bob" } };
        List<Order> orders = new List<Order> { new Order { id = 100, customerId = 1 }, new Order { id = 101, customerId = 2 } };
        List<Item> items = new List<Item> { new Item { orderId = 100, sku = "a" } };
        List<Result> result = new Func<List<Result>>(() => {
    var _res = new List<Result>();
    foreach (var o in orders) {
        foreach (var c in customers) {
            if (!((o.customerId == c.id))) continue;
            foreach (var i in items) {
                if (!((o.id == i.orderId))) continue;
                _res.Add(new Result { orderId = o.id, name = c.name, item = i });
            }
        }
    }
    return _res;
})();
        Console.WriteLine("--- Left Join Multi ---");
        foreach (var r in result) {
            Console.WriteLine(string.Join(" ", new [] { Convert.ToString(r.orderId), Convert.ToString(r.name), Convert.ToString(r.item) }));
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
        public int orderId;
        public string name;
        public Item item;
    }
    
    
    
}
