using System;
using System.Collections.Generic;
using System.Linq;

class Program
{
    static void Main()
    {
        var products = new dynamic[] { new Dictionary<string, dynamic> { { "name", "Laptop" }, { "price", 1500 } }, new Dictionary<string, dynamic> { { "name", "Smartphone" }, { "price", 900 } }, new Dictionary<string, dynamic> { { "name", "Tablet" }, { "price", 600 } }, new Dictionary<string, dynamic> { { "name", "Monitor" }, { "price", 300 } }, new Dictionary<string, dynamic> { { "name", "Keyboard" }, { "price", 100 } }, new Dictionary<string, dynamic> { { "name", "Mouse" }, { "price", 50 } }, new Dictionary<string, dynamic> { { "name", "Headphones" }, { "price", 200 } } };
        var expensive = products.OrderBy(p => (-p.price)).Skip(1).Take(3).Select(p => p).ToArray();
        Console.WriteLine("--- Top products (excluding most expensive) ---");
        foreach (var item in expensive)
        {
            Console.WriteLine(string.Join(" ", new[] { Convert.ToString(item.name), Convert.ToString("costs $"), Convert.ToString(item.price) }));
        }
    }
}
