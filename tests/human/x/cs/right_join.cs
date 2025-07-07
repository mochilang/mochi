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
            new Customer { id = 4, name = "Diana" },
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
        };
        var result =
            from c in customers
            join o in orders on c.id equals o.customerId into os
            from o in os.DefaultIfEmpty()
            select new { customerName = c.name, order = o };
        Console.WriteLine("--- Right Join using LINQ ---");
        foreach (var entry in result)
        {
            if (entry.order != null)
            {
                Console.WriteLine(
                    $"Customer {entry.customerName} has order {entry.order.id} - ${entry.order.total}"
                );
            }
            else
            {
                Console.WriteLine($"Customer {entry.customerName} has no orders");
            }
        }
    }
}
