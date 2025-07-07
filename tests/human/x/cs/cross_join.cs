using System;
using System.Collections.Generic;
using System.Linq;

class Program
{
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
        };
        var result =
            from o in orders
            from c in customers
            select new
            {
                orderId = o.id,
                orderCustomerId = o.customerId,
                pairedCustomerName = c.name,
                orderTotal = o.total,
            };
        Console.WriteLine("--- Cross Join: All order-customer pairs ---");
        foreach (var entry in result)
        {
            Console.WriteLine(
                $"Order {entry.orderId} (customerId: {entry.orderCustomerId}, total: ${entry.orderTotal}) paired with {entry.pairedCustomerName}"
            );
        }
    }
}
