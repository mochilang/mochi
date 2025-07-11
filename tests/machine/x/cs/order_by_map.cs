using System;
using System.Collections.Generic;
using System.Linq;

class Program {
    static void Main() {
        List<Data> data = new List<Data> { new Data { a = 1, b = 2 }, new Data { a = 1, b = 1 }, new Data { a = 0, b = 5 } };
        List<Data> sorted = data.OrderBy(x => new Data { a = x.a, b = x.b }).Select(x => x).ToArray();
        Console.WriteLine(JsonSerializer.Serialize(sorted));
    }
    public class Data {
        public int a;
        public int b;
    }
    
    
}
