using System;
using System.Collections.Generic;
using System.Linq;

class Program {
    static List<List<string>> Ladders(string begin, string end, List<string> words) {
        var wordSet = new HashSet<string>(words);
        if (!wordSet.Contains(end)) return new List<List<string>>();
        var parents = new Dictionary<string, List<string>>();
        var level = new HashSet<string> { begin };
        var visited = new HashSet<string> { begin };
        bool found = false;
        while (level.Count > 0 && !found) {
            var next = new HashSet<string>();
            foreach (var word in level.OrderBy(x => x)) {
                var arr = word.ToCharArray();
                for (int i = 0; i < arr.Length; i++) {
                    char orig = arr[i];
                    for (char c = 'a'; c <= 'z'; c++) {
                        if (c == orig) continue;
                        arr[i] = c;
                        var nw = new string(arr);
                        if (!wordSet.Contains(nw) || visited.Contains(nw)) continue;
                        next.Add(nw);
                        if (!parents.ContainsKey(nw)) parents[nw] = new List<string>();
                        parents[nw].Add(word);
                        if (nw == end) found = true;
                    }
                    arr[i] = orig;
                }
            }
            foreach (var w in next) visited.Add(w);
            level = next;
        }
        var outp = new List<List<string>>();
        if (!found) return outp;
        var path = new List<string> { end };
        Dfs(end, begin, parents, path, outp);
        outp = outp.OrderBy(p => string.Join("->", p)).ToList();
        return outp;
    }

    static void Dfs(string word, string begin, Dictionary<string, List<string>> parents, List<string> path, List<List<string>> outp) {
        if (word == begin) {
            var seq = new List<string>(path);
            seq.Reverse();
            outp.Add(seq);
            return;
        }
        var plist = parents[word].OrderBy(x => x).ToList();
        foreach (var p in plist) {
            path.Add(p);
            Dfs(p, begin, parents, path, outp);
            path.RemoveAt(path.Count - 1);
        }
    }

    static string Fmt(List<List<string>> paths) {
        var lines = new List<string> { paths.Count.ToString() };
        foreach (var p in paths) lines.Add(string.Join("->", p));
        return string.Join("\n", lines);
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
            string begin = lines[idx++], end = lines[idx++];
            int n = int.Parse(lines[idx++]);
            var words = new List<string>();
            for (int i = 0; i < n; i++) words.Add(lines[idx++]);
            outp.Add(Fmt(Ladders(begin, end, words)));
        }
        Console.Write(string.Join("\n\n", outp));
    }
}
