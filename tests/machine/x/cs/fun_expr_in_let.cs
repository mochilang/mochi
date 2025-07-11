using System;

class Program
{
    static void Main()
    {
        Func<int, int> square = new Func<long, long>((long x) =>
        {
            return (x * x);
        });
        Console.WriteLine(square(6));
    }
}
