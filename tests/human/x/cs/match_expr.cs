using System;

class Program
{
    static void Main()
    {
        int x = 2;
        string label = x switch
        {
            1 => "one",
            2 => "two",
            3 => "three",
            _ => "unknown",
        };
        Console.WriteLine(label);
    }
}
