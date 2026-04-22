using System;
using System.Collections.Generic;
using System.Linq;

class Program {
    static int LadderLength(string begin, string end, List<string> words) {
        var wordSet = new HashSet<string>(words);
        if (!wordSet.Contains(end)) return 0;
        var level = new HashSet<string> { begin };
        var visited = new HashSet<string> { begin };
        int steps = 1;
        while (level.Count > 0) {
            if (level.Contains(end)) return steps;
            var next = new HashSet<string>();
            foreach (var word in level.OrderBy(x => x)) {
                var arr = word.ToCharArray();
                for (int i = 0; i < arr.Length; i++) {
                    char orig = arr[i];
                    for (char c = 'a'; c <= 'z'; c++) {
                        if (c == orig) continue;
                        arr[i] = c;
                        var nw = new string(arr);
                        if (wordSet.Contains(nw) && !visited.Contains(nw)) next.Add(nw);
                    }
                    arr[i] = orig;
                }
            }
            foreach (var word in next) visited.Add(word);
            level = next;
            steps++;
        }
        return 0;
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
            string begin = lines[idx++];
            string end = lines[idx++];
            int n = int.Parse(lines[idx++]);
            var words = new List<string>();
            for (int i = 0; i < n; i++) words.Add(lines[idx++]);
            outp.Add(LadderLength(begin, end, words).ToString());
        }
        Console.Write(string.Join("\n\n", outp));
    }
}
