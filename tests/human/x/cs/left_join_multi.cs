using System;
using System.Collections.Generic;
using System.Linq;

class Customer
{
    public int id;
    public string name;
}

class Order
{
    public int id;
    public int customerId;
}

class Item
{
    public int orderId;
    public string sku;
}

class Program
{
    static void Main()
    {
        var customers = new List<Customer>
        {
            new Customer { id = 1, name = "Alice" },
            new Customer { id = 2, name = "Bob" },
        };
        var orders = new List<Order>
        {
            new Order { id = 100, customerId = 1 },
            new Order { id = 101, customerId = 2 },
        };
        var items = new List<Item>
        {
            new Item { orderId = 100, sku = "a" },
        };

        var result =
            from o in orders
            join c in customers on o.customerId equals c.id
            join i in items on o.id equals i.orderId into iset
            from i in iset.DefaultIfEmpty()
            select new
            {
                orderId = o.id,
                name = c.name,
                item = i,
            };

        Console.WriteLine("--- Left Join Multi ---");
        foreach (var r in result)
        {
            Console.WriteLine($"{r.orderId} {r.name} {r.item?.sku}");
        }
    }
}
