using System;
using System.Collections.Generic;

class Program {
    static int MinCut(string s) {
        int n = s.Length;
        var pal = new bool[n, n];
        var cuts = new int[n];
        for (int end = 0; end < n; end++) {
            cuts[end] = end;
            for (int start = 0; start <= end; start++) {
                if (s[start] == s[end] && (end - start <= 2 || pal[start + 1, end - 1])) {
                    pal[start, end] = true;
                    if (start == 0) cuts[end] = 0;
                    else cuts[end] = Math.Min(cuts[end], cuts[start - 1] + 1);
                }
            }
        }
        return cuts[n - 1];
    }

    static void Main() {
        var lines = new List<string>();
        string line;
        while ((line = Console.ReadLine()) != null) lines.Add(line);
        if (lines.Count == 0) return;
        int tc = int.Parse(lines[0]);
        var outp = new List<string>();
        for (int i = 1; i <= tc; i++) outp.Add(MinCut(lines[i]).ToString());
        Console.Write(string.Join("\n\n", outp));
    }
}
