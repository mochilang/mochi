using System;
using System.Collections.Generic;

class Program {
    static int Dfs(int i, int[] vals, bool[] ok, ref int best) {
        if (i >= vals.Length || !ok[i]) return 0;
        int left = Math.Max(0, Dfs(2 * i + 1, vals, ok, ref best));
        int right = Math.Max(0, Dfs(2 * i + 2, vals, ok, ref best));
        best = Math.Max(best, vals[i] + left + right);
        return vals[i] + Math.Max(left, right);
    }

    static int Solve(int[] vals, bool[] ok) {
        int best = -1000000000;
        Dfs(0, vals, ok, ref best);
        return best;
    }

    static void Main() {
        var lines = new List<string>();
        string line;
        while ((line = Console.ReadLine()) != null) lines.Add(line);
        if (lines.Count == 0) return;
        int tc = int.Parse(lines[0].Trim());
        int idx = 1;
        var outLines = new List<string>();
        for (int t = 0; t < tc; t++) {
            int n = int.Parse(lines[idx++].Trim());
            var vals = new int[n];
            var ok = new bool[n];
            for (int i = 0; i < n; i++) {
                string tok = lines[idx++].Trim();
                if (tok != "null") { ok[i] = true; vals[i] = int.Parse(tok); }
            }
            outLines.Add(Solve(vals, ok).ToString());
        }
        Console.Write(string.Join("\n", outLines));
    }
}
