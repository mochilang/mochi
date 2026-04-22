using System;
using System.Collections.Generic;

public class Program
{
    static long CountDigitOne(long n)
    {
        long total = 0;
        for (long m = 1; m <= n; m *= 10)
        {
            long high = n / (m * 10);
            long cur = (n / m) % 10;
            long low = n % m;
            if (cur == 0) total += high * m;
            else if (cur == 1) total += high * m + low + 1;
            else total += (high + 1) * m;
        }
        return total;
    }

    public static void Main()
    {
        string first = Console.ReadLine();
        if (first == null) return;
        int t = int.Parse(first.Trim());
        var outLines = new List<string>();
        for (int i = 0; i < t; i++)
        {
            outLines.Add(CountDigitOne(long.Parse((Console.ReadLine() ?? "0").Trim())).ToString());
        }
        Console.Write(string.Join("\n", outLines));
    }
}
