using System;
using System.Collections.Generic;

class Program {
    static string Quote(string s) {
        return "\"" + s + "\"";
    }

    static void Main() {
        var lines = new List<string>();
        string line;
        while ((line = Console.ReadLine()) != null) lines.Add(line);
        if (lines.Count == 0) return;
        int tc = int.Parse(lines[0]);
        int idx = 1;
        var outp = new List<string>();
        for (int t = 0; t < tc; t++) {
            string data = lines[idx++];
            int q = int.Parse(lines[idx++]);
            int pos = 0;
            var block = new List<string> { q.ToString() };
            for (int i = 0; i < q; i++) {
                int n = int.Parse(lines[idx++]);
                int len = Math.Min(n, data.Length - pos);
                string part = data.Substring(pos, len);
                pos += len;
                block.Add(Quote(part));
            }
            outp.Add(string.Join("\n", block));
        }
        Console.Write(string.Join("\n\n", outp));
    }
}
