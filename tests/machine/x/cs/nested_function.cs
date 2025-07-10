using System;

class Program {
    static long outer(long x) {
        long inner(long y) {
            return (x + y);
        }
        return inner(5);
    }
    
    static void Main() {
        Console.WriteLine(outer(3));
    }
}
