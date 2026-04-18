using System;
using System.Collections.Generic;

public static class Program
{
    static List<int> AddLists(List<int> a, List<int> b)
    {
        var outList = new List<int>();
        int i = 0, j = 0, carry = 0;
        while (i < a.Count || j < b.Count || carry > 0)
        {
            int sum = carry;
            if (i < a.Count) sum += a[i++];
            if (j < b.Count) sum += b[j++];
            outList.Add(sum % 10);
            carry = sum / 10;
        }
        return outList;
    }

    static string Format(List<int> a) => "[" + string.Join(",", a) + "]";

    public static void Main()
    {
        string[] parts = Console.In.ReadToEnd().Split((char[])null, StringSplitOptions.RemoveEmptyEntries);
        if (parts.Length == 0) return;
        int idx = 0, t = int.Parse(parts[idx++]);
        var lines = new List<string>();
        for (int tc = 0; tc < t; tc++)
        {
            int n = int.Parse(parts[idx++]);
            var a = new List<int>();
            for (int i = 0; i < n; i++) a.Add(int.Parse(parts[idx++]));
            int m = int.Parse(parts[idx++]);
            var b = new List<int>();
            for (int i = 0; i < m; i++) b.Add(int.Parse(parts[idx++]));
            lines.Add(Format(AddLists(a, b)));
        }
        Console.Write(string.Join("\n", lines));
    }
}
