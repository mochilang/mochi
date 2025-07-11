using System;

class Program {
    static void Main() {
        M m = new M { a = 1, b = 2 };
        Console.WriteLine(JsonSerializer.Serialize(m));
    }
    public class M {
        public int a;
        public int b;
    }
    
    
}
