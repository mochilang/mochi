using System;

class Program {
    static int sum_rec(int n, int acc) {
        if (n == 0) {
            return acc;
        }
        return sum_rec((n - 1), (acc + n));
    }
    
    static void Main() {
        Console.WriteLine(sum_rec(10, 0));
    }
}
