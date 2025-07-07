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
    public int total;
}

class Program
{
    static void Main()
    {
        var customers = new List<Customer>
        {
            new Customer { id = 1, name = "Alice" },
            new Customer { id = 2, name = "Bob" },
            new Customer { id = 3, name = "Charlie" },
        };
        var orders = new List<Order>
        {
            new Order
            {
                id = 100,
                customerId = 1,
                total = 250,
            },
            new Order
            {
                id = 101,
                customerId = 2,
                total = 125,
            },
            new Order
            {
                id = 102,
                customerId = 1,
                total = 300,
            },
            new Order
            {
                id = 103,
                customerId = 4,
                total = 80,
            },
        };

        var result =
            from o in orders
            join c in customers on o.customerId equals c.id
            select new
            {
                orderId = o.id,
                customerName = c.name,
                total = o.total,
            };

        Console.WriteLine("--- Orders with customer info ---");
        foreach (var entry in result)
        {
            Console.WriteLine($"Order {entry.orderId} by {entry.customerName} - ${entry.total}");
        }
    }
}
