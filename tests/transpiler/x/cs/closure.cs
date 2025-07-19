// Mochi 0.10.31 - generated 2025-07-19 16:18:19 UTC
using System;

class Program {
    static Func<int, int> makeAdder(int n) {
    return (int x) => (x + n);
}
    static void Main() {
        var add10 = makeAdder(10);
        Console.WriteLine(add10(7));
    }
}
