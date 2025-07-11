using System;

class Program {
    static Func<int, int> makeAdder(int n) {
        return new Func<int, int>((int x) => {
    return (x + n);
});
    }
    
    static void Main() {
        Func<int, int> add10 = makeAdder(10);
        Console.WriteLine(add10(7));
    }
}
