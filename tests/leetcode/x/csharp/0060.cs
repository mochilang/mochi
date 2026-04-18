using System;
using System.Collections.Generic;

class Program {
    static string GetPermutation(int n, int k) {
        var digits = new List<string>();
        for (int i = 1; i <= n; i++) digits.Add(i.ToString());
        var fact = new int[n + 1];
        fact[0] = 1;
        for (int i = 1; i <= n; i++) fact[i] = fact[i - 1] * i;
        k--;
        var outp = "";
        for (int rem = n; rem >= 1; rem--) {
            int block = fact[rem - 1];
            int idx = k / block;
            k %= block;
            outp += digits[idx];
            digits.RemoveAt(idx);
        }
        return outp;
    }

    static void Main() {
        var lines = Console.In.ReadToEnd().Split(new[] { "\r\n", "\n" }, StringSplitOptions.None);
        if (lines.Length == 0 || lines[0].Trim() == "") return;
        int idx = 0, t = int.Parse(lines[idx++].Trim());
        var outp = new List<string>();
        for (int tc = 0; tc < t; tc++) {
            int n = int.Parse(lines[idx++].Trim());
            int k = int.Parse(lines[idx++].Trim());
            outp.Add(GetPermutation(n, k));
        }
        Console.Write(string.Join("\n", outp));
    }
}
