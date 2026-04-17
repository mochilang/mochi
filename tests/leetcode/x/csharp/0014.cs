using System;
using System.Linq;

public static class Program
{
    static string Lcp(string[] strs)
    {
        string prefix = strs[0];
        while (!strs.All(s => s.StartsWith(prefix))) {
            prefix = prefix.Substring(0, prefix.Length - 1);
        }
        return prefix;
    }

    public static void Main()
    {
        string[] parts = Console.In.ReadToEnd().Split((char[])null, StringSplitOptions.RemoveEmptyEntries);
        if (parts.Length == 0) return;
        int idx = 0;
        int t = int.Parse(parts[idx++]);
        for (int tc = 0; tc < t; tc++) {
            int n = int.Parse(parts[idx++]);
            string[] strs = new string[n];
            for (int i = 0; i < n; i++) strs[i] = parts[idx++];
            Console.WriteLine("\"" + Lcp(strs) + "\"");
        }
    }
}
