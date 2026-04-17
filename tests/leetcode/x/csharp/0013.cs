using System;
using System.Collections.Generic;

public static class Program
{
    static readonly Dictionary<char, int> Values = new Dictionary<char, int> {
        ['I'] = 1, ['V'] = 5, ['X'] = 10, ['L'] = 50, ['C'] = 100, ['D'] = 500, ['M'] = 1000
    };

    static int RomanToInt(string s)
    {
        int total = 0;
        for (int i = 0; i < s.Length; i++)
        {
            int cur = Values[s[i]];
            int next = i + 1 < s.Length ? Values[s[i + 1]] : 0;
            total += cur < next ? -cur : cur;
        }
        return total;
    }

    public static void Main()
    {
        string[] parts = Console.In.ReadToEnd().Split((char[])null, StringSplitOptions.RemoveEmptyEntries);
        if (parts.Length == 0) return;
        int t = int.Parse(parts[0]);
        for (int i = 0; i < t; i++)
        {
            Console.WriteLine(RomanToInt(parts[i + 1]));
        }
    }
}
