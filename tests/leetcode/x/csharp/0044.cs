using System;
using System.Collections.Generic;

class Program
{
    static bool IsMatch(string s, string p)
    {
        int i = 0, j = 0, star = -1, match = 0;
        while (i < s.Length)
        {
            if (j < p.Length && (p[j] == '?' || p[j] == s[i])) { i++; j++; }
            else if (j < p.Length && p[j] == '*') { star = j; match = i; j++; }
            else if (star != -1) { j = star + 1; match++; i = match; }
            else return false;
        }
        while (j < p.Length && p[j] == '*') j++;
        return j == p.Length;
    }

    static void Main()
    {
        var lines = Console.In.ReadToEnd().Split(new[] { "\r\n", "\n" }, StringSplitOptions.None);
        if (lines.Length == 0 || lines[0].Trim() == "") return;
        int idx = 0;
        int t = int.Parse(lines[idx++].Trim());
        var output = new List<string>();
        for (int tc = 0; tc < t; tc++)
        {
            int n = int.Parse(lines[idx++].Trim());
            string s = n > 0 ? lines[idx++] : "";
            int m = int.Parse(lines[idx++].Trim());
            string p = m > 0 ? lines[idx++] : "";
            output.Add(IsMatch(s, p) ? "true" : "false");
        }
        Console.Write(string.Join("\n", output));
    }
}
