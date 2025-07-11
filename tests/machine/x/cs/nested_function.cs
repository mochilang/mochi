using System;

class Program {
    static int outer(int x) {
        int inner(int y) {
            return (x + y);
        }
        return inner(5);
    }
    
    static void Main() {
        Console.WriteLine(outer(3));
    }
}
