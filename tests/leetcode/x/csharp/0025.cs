using System;
using System.Collections.Generic;
class Program {
    static void Main() {
        var lines = Console.In.ReadToEnd().Split(new[]{"\r\n","\n"}, StringSplitOptions.None);
        if (lines.Length == 0 || lines[0].Trim() == "") return;
        int idx = 0, t = int.Parse(lines[idx++].Trim());
        var outp = new List<string>();
        for (int tc = 0; tc < t; tc++) {
            int n = idx < lines.Length ? int.Parse(lines[idx++].Trim()) : 0;
            var arr = new List<int>();
            for (int i = 0; i < n; i++) arr.Add(idx < lines.Length ? int.Parse(lines[idx++].Trim()) : 0);
            int k = idx < lines.Length ? int.Parse(lines[idx++].Trim()) : 1;
            for (int i = 0; i + k <= arr.Count; i += k) for (int l = i, r = i + k - 1; l < r; l++, r--) { int tmp = arr[l]; arr[l] = arr[r]; arr[r] = tmp; }
            outp.Add("[" + string.Join(",", arr) + "]");
        }
        Console.Write(string.Join("\n", outp));
    }
}
