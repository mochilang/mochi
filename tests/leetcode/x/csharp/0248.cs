using System;
using System.Collections.Generic;

public class Program
{
    static readonly char[] PairA = { '0', '1', '6', '8', '9' };
    static readonly char[] PairB = { '0', '1', '9', '8', '6' };

    static List<string> Build(int n, int m)
    {
        if (n == 0) return new List<string> { "" };
        if (n == 1) return new List<string> { "0", "1", "8" };
        var mids = Build(n - 2, m);
        var res = new List<string>();
        foreach (var mid in mids)
        {
            for (int i = 0; i < PairA.Length; i++)
            {
                char a = PairA[i];
                char b = PairB[i];
                if (n == m && a == '0') continue;
                res.Add($"{a}{mid}{b}");
            }
        }
        return res;
    }

    static int CountRange(string low, string high)
    {
        int ans = 0;
        for (int len = low.Length; len <= high.Length; len++)
        {
            foreach (var s in Build(len, len))
            {
                if (len == low.Length && string.CompareOrdinal(s, low) < 0) continue;
                if (len == high.Length && string.CompareOrdinal(s, high) > 0) continue;
                ans++;
            }
        }
        return ans;
    }

    public static void Main()
    {
        string first = Console.ReadLine();
        if (first == null) return;
        int t = int.Parse(first.Trim());
        var outLines = new List<string>();
        for (int i = 0; i < t; i++)
        {
            string low = (Console.ReadLine() ?? "").Trim();
            string high = (Console.ReadLine() ?? "").Trim();
            outLines.Add(CountRange(low, high).ToString());
        }
        Console.Write(string.Join("\n", outLines));
    }
}
