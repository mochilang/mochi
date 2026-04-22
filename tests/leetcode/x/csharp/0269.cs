using System;
using System.Collections.Generic;
using System.Linq;

public class Program
{
    static string Solve(string[] words)
    {
        var chars = new SortedSet<char>(string.Concat(words));
        var adj = new Dictionary<char, SortedSet<char>>();
        var indeg = new Dictionary<char, int>();
        foreach (char c in chars)
        {
            adj[c] = new SortedSet<char>();
            indeg[c] = 0;
        }
        for (int i = 0; i + 1 < words.Length; i++)
        {
            string a = words[i], b = words[i + 1];
            int m = Math.Min(a.Length, b.Length);
            if (a.Substring(0, m) == b.Substring(0, m) && a.Length > b.Length) return "";
            for (int j = 0; j < m; j++)
            {
                if (a[j] != b[j])
                {
                    if (adj[a[j]].Add(b[j])) indeg[b[j]]++;
                    break;
                }
            }
        }
        var zeros = new SortedSet<char>(chars.Where(c => indeg[c] == 0));
        var outChars = new List<char>();
        while (zeros.Count > 0)
        {
            char c = zeros.Min;
            zeros.Remove(c);
            outChars.Add(c);
            foreach (char nei in adj[c])
            {
                indeg[nei]--;
                if (indeg[nei] == 0) zeros.Add(nei);
            }
        }
        return outChars.Count == chars.Count ? new string(outChars.ToArray()) : "";
    }

    public static void Main()
    {
        string first = Console.ReadLine();
        if (first == null) return;
        int t = int.Parse(first.Trim());
        var outLines = new List<string>();
        for (int tc = 0; tc < t; tc++)
        {
            int n = int.Parse((Console.ReadLine() ?? "0").Trim());
            var words = new string[n];
            for (int i = 0; i < n; i++) words[i] = (Console.ReadLine() ?? "").Trim();
            outLines.Add(Solve(words));
        }
        Console.Write(string.Join("\n", outLines));
    }
}
