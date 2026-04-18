using System;
using System.Collections.Generic;

public static class Program {
    static int Longest(string s) {
        var last = new Dictionary<char, int>();
        int left = 0, best = 0;
        for (int right = 0; right < s.Length; right++) {
            char ch = s[right];
            if (last.ContainsKey(ch) && last[ch] >= left) left = last[ch] + 1;
            last[ch] = right;
            best = Math.Max(best, right - left + 1);
        }
        return best;
    }
    public static void Main() {
        string first = Console.ReadLine();
        if (string.IsNullOrEmpty(first)) return;
        int t = int.Parse(first.Trim());
        var lines = new List<string>();
        for (int i = 0; i < t; i++) lines.Add(Longest(Console.ReadLine() ?? "").ToString());
        Console.Write(string.Join("\n", lines));
    }
}
