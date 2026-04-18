using System;
using System.Collections.Generic;

class Program {
    static int Solve(List<List<int>> tri) {
        var dp = new List<int>(tri[tri.Count - 1]);
        for (int i = tri.Count - 2; i >= 0; i--)
            for (int j = 0; j <= i; j++)
                dp[j] = tri[i][j] + Math.Min(dp[j], dp[j + 1]);
        return dp[0];
    }

    static void Main() {
        var toks = Console.In.ReadToEnd().Split((char[])null, StringSplitOptions.RemoveEmptyEntries);
        if (toks.Length == 0) return;
        int idx = 0, t = int.Parse(toks[idx++]);
        var outLines = new string[t];
        for (int tc = 0; tc < t; tc++) {
            int rows = int.Parse(toks[idx++]);
            var tri = new List<List<int>>();
            for (int r = 1; r <= rows; r++) {
                var row = new List<int>();
                for (int j = 0; j < r; j++) row.Add(int.Parse(toks[idx++]));
                tri.Add(row);
            }
            outLines[tc] = Solve(tri).ToString();
        }
        Console.Write(string.Join("\n", outLines));
    }
}
