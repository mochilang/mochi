using System;
using System.Collections.Generic;

class Program {
    static int MaxArea(int[] h) {
        int left = 0, right = h.Length - 1, best = 0;
        while (left < right) {
            int height = Math.Min(h[left], h[right]);
            best = Math.Max(best, (right - left) * height);
            if (h[left] < h[right]) left++; else right--;
        }
        return best;
    }

    static void Main() {
        var lines = new List<string>();
        string line;
        while ((line = Console.ReadLine()) != null) lines.Add(line);
        if (lines.Count == 0) return;
        int t = int.Parse(lines[0].Trim());
        int idx = 1;
        var outLines = new List<string>();
        for (int tc = 0; tc < t; tc++) {
            int n = int.Parse(lines[idx++].Trim());
            var h = new int[n];
            for (int i = 0; i < n; i++) h[i] = int.Parse(lines[idx++].Trim());
            outLines.Add(MaxArea(h).ToString());
        }
        Console.Write(string.Join("\n", outLines));
    }
}
