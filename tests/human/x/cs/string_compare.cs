using System;

class Program
{
    static void Main()
    {
        Console.WriteLine(string.Compare("a", "b") < 0);
        Console.WriteLine(string.Compare("a", "a") <= 0);
        Console.WriteLine(string.Compare("b", "a") > 0);
        Console.WriteLine(string.Compare("b", "b") >= 0);
    }
}
