using System;
using System.Collections.Generic;
using System.Linq;

class Program {
    static void Main() {
        List<Product> products = new List<Product> { new Product { name = "Laptop", price = 1500 }, new Product { name = "Smartphone", price = 900 }, new Product { name = "Tablet", price = 600 }, new Product { name = "Monitor", price = 300 }, new Product { name = "Keyboard", price = 100 }, new Product { name = "Mouse", price = 50 }, new Product { name = "Headphones", price = 200 } };
        List<Product> expensive = products.OrderBy(p => (-p.price)).Skip(1).Take(3).Select(p => p).ToArray();
        Console.WriteLine("--- Top products (excluding most expensive) ---");
        foreach (var item in expensive) {
            Console.WriteLine(string.Join(" ", new [] { Convert.ToString(item.name), Convert.ToString("costs $"), Convert.ToString(item.price) }));
        }
    }
    public class Product {
        public string name;
        public int price;
    }
    
    
}
