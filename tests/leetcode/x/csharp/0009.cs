using System;

public static class Program
{
    static bool IsPalindrome(int x)
    {
        if (x < 0) return false;
        int original = x;
        long rev = 0;
        while (x > 0)
        {
            rev = rev * 10 + (x % 10);
            x /= 10;
        }
        return rev == original;
    }

    public static void Main()
    {
        string input = Console.In.ReadToEnd();
        string[] parts = input.Split((char[])null, StringSplitOptions.RemoveEmptyEntries);
        if (parts.Length == 0) return;
        int idx = 0;
        int t = int.Parse(parts[idx++]);
        for (int i = 0; i < t; i++)
        {
            int x = int.Parse(parts[idx++]);
            Console.WriteLine(IsPalindrome(x) ? "true" : "false");
        }
    }
}
