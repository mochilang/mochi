using System;
using System.Collections.Generic;
using System.Linq;

class Product { public string name; public int price; }

class Program {
    static void Main() {
        var products = new List<Product> {
            new Product{ name="Laptop", price=1500 },
            new Product{ name="Smartphone", price=900 },
            new Product{ name="Tablet", price=600 },
            new Product{ name="Monitor", price=300 },
            new Product{ name="Keyboard", price=100 },
            new Product{ name="Mouse", price=50 },
            new Product{ name="Headphones", price=200 }
        };
        var expensive = products
            .OrderByDescending(p => p.price)
            .Skip(1)
            .Take(3)
            .Select(p => p);
        Console.WriteLine("--- Top products (excluding most expensive) ---");
        foreach (var item in expensive) {
            Console.WriteLine($"{item.name} costs ${item.price}");
        }
    }
}
