using System;
using System.Collections.Generic;

class Program {
    static int Solve(string s, string t) {
        var dp = new int[t.Length + 1];
        dp[0] = 1;
        foreach (char ch in s) {
            for (int j = t.Length; j >= 1; j--) {
                if (ch == t[j - 1]) dp[j] += dp[j - 1];
            }
        }
        return dp[t.Length];
    }

    static void Main() {
        var lines = new List<string>();
        string line;
        while ((line = Console.ReadLine()) != null) lines.Add(line);
        if (lines.Count == 0) return;
        int tc = int.Parse(lines[0].Trim());
        var outLines = new List<string>();
        for (int i = 0; i < tc; i++) {
            outLines.Add(Solve(lines[1 + 2 * i], lines[2 + 2 * i]).ToString());
        }
        Console.Write(string.Join("\n", outLines));
    }
}
