using System;

class Program
{
    static void Main()
    {
        int x = 1 + 2;
        if (x != 3)
            throw new Exception("addition failed");
        Console.WriteLine("ok");
    }
}
