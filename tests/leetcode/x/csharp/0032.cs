using System;
using System.Collections.Generic;

class Program {
    static int SolveCase(string s) {
        var stack = new List<int> { -1 };
        int best = 0;
        for (int i = 0; i < s.Length; i++) {
            if (s[i] == '(') {
                stack.Add(i);
            } else {
                stack.RemoveAt(stack.Count - 1);
                if (stack.Count == 0) {
                    stack.Add(i);
                } else {
                    best = Math.Max(best, i - stack[stack.Count - 1]);
                }
            }
        }
        return best;
    }

    static void Main() {
        var lines = Console.In.ReadToEnd().Split(new[] { "\r\n", "\n" }, StringSplitOptions.None);
        if (lines.Length == 0 || lines[0].Trim() == "") return;
        int idx = 0;
        int t = int.Parse(lines[idx++].Trim());
        var results = new List<string>();
        for (int tc = 0; tc < t; tc++) {
            int n = idx < lines.Length ? int.Parse(lines[idx++].Trim()) : 0;
            string s = n > 0 && idx < lines.Length ? lines[idx++] : "";
            results.Add(SolveCase(s).ToString());
        }
        Console.Write(string.Join("\n", results));
    }
}
