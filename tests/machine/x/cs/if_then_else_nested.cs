using System;

class Program {
    static void Main() {
        long x = 8;
        string msg = ((x > 10) ? "big" : ((x > 5) ? "medium" : "small"));
        Console.WriteLine(msg);
    }
}

