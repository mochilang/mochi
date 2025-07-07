using System;

class Program
{
    static void Main()
    {
        string prefix = "fore";
        string s1 = "forest";
        Console.WriteLine(s1.Substring(0, prefix.Length) == prefix);
        string s2 = "desert";
        Console.WriteLine(s2.Substring(0, prefix.Length) == prefix);
    }
}
