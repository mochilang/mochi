using System;
using System.Collections.Generic;
class Program {
    static void Main() {
        var lines = Console.In.ReadToEnd().Split(new[] {"\r\n", "\n"}, StringSplitOptions.None);
        if (lines.Length == 0 || lines[0].Trim() == "") return;
        int idx = 0, t = int.Parse(lines[idx++].Trim());
        var outp = new List<string>();
        for (int tc = 0; tc < t; tc++) {
            int k = idx < lines.Length ? int.Parse(lines[idx++].Trim()) : 0;
            var vals = new List<int>();
            for (int i = 0; i < k; i++) {
                int n = idx < lines.Length ? int.Parse(lines[idx++].Trim()) : 0;
                for (int j = 0; j < n; j++) vals.Add(idx < lines.Length ? int.Parse(lines[idx++].Trim()) : 0);
            }
            vals.Sort();
            outp.Add("[" + string.Join(",", vals) + "]");
        }
        Console.Write(string.Join("\n", outp));
    }
}
