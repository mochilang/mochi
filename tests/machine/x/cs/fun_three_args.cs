using System;

class Program {
    static int sum3(int a, int b, int c) {
        return ((a + b) + c);
    }
    
    static void Main() {
        Console.WriteLine(sum3(1, 2, 3));
    }
}
