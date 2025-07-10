using System;

class Program {
    static long sum_rec(long n, long acc) {
        if (n == 0) {
            return acc;
        }
        return sum_rec((n - 1), (acc + n));
    }
    
    static void Main() {
        Console.WriteLine(sum_rec(10, 0));
    }
}
