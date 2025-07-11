using System;

public record struct Counter {
    public int n;
}

class Program {
    static void inc(Counter c) {
        c.n = (c.n + 1);
    }
    
    static void Main() {
        Counter c = new Counter { n = 0 };
        inc(c);
        Console.WriteLine(c.n);
    }
}
