using System;

class Program {
    static int Hist(int[] h) {
        int best = 0;
        for (int i = 0; i < h.Length; i++) {
            int mn = h[i];
            for (int j = i; j < h.Length; j++) {
                if (h[j] < mn) mn = h[j];
                int area = mn * (j - i + 1);
                if (area > best) best = area;
            }
        }
        return best;
    }

    static void Main() {
        var toks = Console.In.ReadToEnd().Split((char[])null, StringSplitOptions.RemoveEmptyEntries);
        if (toks.Length == 0) return;
        int idx = 0, t = int.Parse(toks[idx++]);
        var outLines = new string[t];
        for (int tc = 0; tc < t; tc++) {
            int rows = int.Parse(toks[idx++]), cols = int.Parse(toks[idx++]);
            var h = new int[cols];
            int best = 0;
            for (int r = 0; r < rows; r++) {
                string s = toks[idx++];
                for (int c = 0; c < cols; c++) h[c] = s[c] == '1' ? h[c] + 1 : 0;
                best = Math.Max(best, Hist(h));
            }
            outLines[tc] = best.ToString();
        }
        Console.Write(string.Join("\n", outLines));
    }
}
