using System;
using System.Collections.Generic;
using System.Linq;

class Program {
    static void Main() {
        List<Item> items = new List<Item> { new Item { n = 1, v = "a" }, new Item { n = 1, v = "b" }, new Item { n = 2, v = "c" } };
        List<string> result = items.OrderBy(i => i.n).Select(i => i.v).ToArray();
        Console.WriteLine("[" + string.Join(", ", result) + "]");
    }
    public class Item {
        public int n;
        public string v;
    }
    
    
}
