using System;

class Program {
    static void Main() {
        Func<int, int> square = new Func<int, int>((int x) => {
    return (x * x);
});
        Console.WriteLine(square(6));
    }
}
