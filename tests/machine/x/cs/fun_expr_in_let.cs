using System;

class Program
{
    static void Main()
    {
        Func<long, long> square = new Func<long, long>((long x) =>
        {
            return (x * x);
        });
        Console.WriteLine(square(6));
    }
}
