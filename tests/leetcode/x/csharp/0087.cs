using System;
using System.Collections.Generic;

class Program {
    static string s1 = "", s2 = "";
    static Dictionary<string, bool> memo = new Dictionary<string, bool>();

    static bool Dfs(int i1, int i2, int len) {
        string key = i1 + "," + i2 + "," + len;
        if (memo.ContainsKey(key)) return memo[key];
        string a = s1.Substring(i1, len), b = s2.Substring(i2, len);
        if (a == b) return memo[key] = true;
        var cnt = new int[26];
        for (int i = 0; i < len; i++) { cnt[a[i] - 'a']++; cnt[b[i] - 'a']--; }
        foreach (var v in cnt) if (v != 0) return memo[key] = false;
        for (int k = 1; k < len; k++) {
            if ((Dfs(i1, i2, k) && Dfs(i1 + k, i2 + k, len - k)) ||
                (Dfs(i1, i2 + len - k, k) && Dfs(i1 + k, i2, len - k))) return memo[key] = true;
        }
        return memo[key] = false;
    }

    static void Main() {
        var lines = Console.In.ReadToEnd().Split(new[] {"\r\n", "\n"}, StringSplitOptions.None);
        if (lines.Length == 0 || lines[0].Trim().Length == 0) return;
        int t = int.Parse(lines[0].Trim());
        var outLines = new string[t];
        for (int i = 0; i < t; i++) {
            s1 = lines[1 + 2 * i];
            s2 = lines[2 + 2 * i];
            memo.Clear();
            outLines[i] = Dfs(0, 0, s1.Length) ? "true" : "false";
        }
        Console.Write(string.Join("\n", outLines));
    }
}
