using System;

class Program {
    static bool Solve(string s1, string s2, string s3) {
        int m = s1.Length, n = s2.Length;
        if (m + n != s3.Length) return false;
        var dp = new bool[m + 1, n + 1];
        dp[0, 0] = true;
        for (int i = 0; i <= m; i++) {
            for (int j = 0; j <= n; j++) {
                if (i > 0 && dp[i - 1, j] && s1[i - 1] == s3[i + j - 1]) dp[i, j] = true;
                if (j > 0 && dp[i, j - 1] && s2[j - 1] == s3[i + j - 1]) dp[i, j] = true;
            }
        }
        return dp[m, n];
    }

    static void Main() {
        var lines = Console.In.ReadToEnd().Split(new[] { "\r\n", "\n" }, StringSplitOptions.None);
        if (lines.Length == 0 || lines[0].Trim().Length == 0) return;
        int t = int.Parse(lines[0].Trim());
        var outLines = new string[t];
        for (int i = 0; i < t; i++) outLines[i] = Solve(lines[1 + 3 * i], lines[2 + 3 * i], lines[3 + 3 * i]) ? "true" : "false";
        Console.Write(string.Join("\n", outLines));
    }
}
