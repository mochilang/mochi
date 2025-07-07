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
            new Order
            {
                id = 103,
                customerId = 5,
                total = 80,
            },
        };
        var left =
            from o in orders
            join c in customers on o.customerId equals c.id into temp
            from c in temp.DefaultIfEmpty()
            select new { order = o, customer = c };
        var rightOnly =
            from c in customers
            join o in orders on c.id equals o.customerId into os
            from o in os.DefaultIfEmpty()
            where o == null
            select new { order = (Order)null, customer = c };
        var result = left.Concat(rightOnly);
        Console.WriteLine("--- Outer Join using LINQ ---");
        foreach (var row in result)
        {
            if (row.order != null)
            {
                if (row.customer != null)
                {
                    Console.WriteLine(
                        $"Order {row.order.id} by {row.customer.name} - ${row.order.total}"
                    );
                }
                else
                {
                    Console.WriteLine($"Order {row.order.id} by Unknown - ${row.order.total}");
                }
            }
            else
            {
                Console.WriteLine($"Customer {row.customer.name} has no orders");
            }
        }
    }
}
