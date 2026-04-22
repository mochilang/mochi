using System;

class Program {
    static string Solve(string s) {
        char[] chars = s.ToCharArray();
        Array.Reverse(chars);
        string rev = new string(chars);
        string combined = s + "#" + rev;
        int[] pi = new int[combined.Length];
        for (int i = 1; i < combined.Length; i++) {
            int j = pi[i - 1];
            while (j > 0 && combined[i] != combined[j]) j = pi[j - 1];
            if (combined[i] == combined[j]) j++;
            pi[i] = j;
        }
        int keep = pi.Length == 0 ? 0 : pi[pi.Length - 1];
        return rev.Substring(0, s.Length - keep) + s;
    }

    static void Main() {
        var lines = Console.In.ReadToEnd().Split(new[] { "\r\n", "\n" }, StringSplitOptions.None);
        if (lines.Length == 0 || lines[0].Trim().Length == 0) return;
        int t = int.Parse(lines[0].Trim());
        string[] outLines = new string[t];
        for (int i = 0; i < t; i++) {
            string s = i + 1 < lines.Length ? lines[i + 1] : "";
            outLines[i] = Solve(s);
        }
        Console.Write(string.Join("\n", outLines));
    }
}
