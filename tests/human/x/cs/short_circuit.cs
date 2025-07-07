using System;

class Program
{
    static bool Boom(int a, int b)
    {
        Console.WriteLine("boom");
        return true;
    }

    static void Main()
    {
        Console.WriteLine(false && Boom(1, 2));
        Console.WriteLine(true || Boom(1, 2));
    }
}
