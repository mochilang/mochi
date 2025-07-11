using System;

class Program {
    static long add(long a, long b) {
        return (a + b);
    }
    
    static void Main() {
        int add5 = new Func<int, int>((int p0) => { return add(5, p0); });
        Console.WriteLine(add5(3));
    }
}
