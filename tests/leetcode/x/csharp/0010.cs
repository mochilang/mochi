using System;

class Program {
    static bool MatchAt(string s, string p, int i, int j) {
        if (j == p.Length) return i == s.Length;
        bool first = i < s.Length && (p[j] == '.' || s[i] == p[j]);
        if (j + 1 < p.Length && p[j + 1] == '*') {
            return MatchAt(s, p, i, j + 2) || (first && MatchAt(s, p, i + 1, j));
        }
        return first && MatchAt(s, p, i + 1, j + 1);
    }

    static void Main() {
        string line = Console.ReadLine();
        if (line == null) return;
        int t = int.Parse(line.Trim());
        for (int tc = 0; tc < t; tc++) {
            string s = Console.ReadLine() ?? "";
            string p = Console.ReadLine() ?? "";
            Console.Write(MatchAt(s, p, 0, 0) ? "true" : "false");
            if (tc + 1 < t) Console.Write("\n");
        }
    }
}
