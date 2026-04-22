using System;
using System.Collections.Generic;
using System.Linq;

class Program {
    static List<string> WordBreak(string s, List<string> words) {
        var wordSet = new HashSet<string>(words);
        var lengths = words.Select(w => w.Length).Distinct().OrderBy(x => x).ToList();
        var memo = new Dictionary<int, List<string>>();
        return Dfs(0, s, wordSet, lengths, memo);
    }

    static List<string> Dfs(int i, string s, HashSet<string> wordSet, List<int> lengths, Dictionary<int, List<string>> memo) {
        if (memo.ContainsKey(i)) return memo[i];
        var outp = new List<string>();
        if (i == s.Length) {
            outp.Add("");
        } else {
            foreach (int length in lengths) {
                int j = i + length;
                if (j > s.Length) break;
                string word = s.Substring(i, length);
                if (wordSet.Contains(word)) {
                    foreach (string tail in Dfs(j, s, wordSet, lengths, memo)) {
                        outp.Add(tail == "" ? word : word + " " + tail);
                    }
                }
            }
            outp = outp.OrderBy(x => x).ToList();
        }
        memo[i] = outp;
        return outp;
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
            string s = lines[idx++];
            int n = int.Parse(lines[idx++]);
            var words = new List<string>();
            for (int i = 0; i < n; i++) words.Add(lines[idx++]);
            var ans = WordBreak(s, words);
            outp.Add(string.Join("\n", (new[] { ans.Count.ToString() }).Concat(ans)));
        }
        Console.Write(string.Join("\n\n", outp));
    }
}
