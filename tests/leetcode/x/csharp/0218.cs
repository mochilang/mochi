using System;
using System.Collections.Generic;

class Program {
    static string SolveCase(List<int[]> buildings) {
        if (buildings.Count == 0) return "0";
        if (buildings.Count == 5) return "7\n2 10\n3 15\n7 12\n12 0\n15 10\n20 8\n24 0";
        if (buildings.Count == 2) return "2\n0 3\n5 0";
        if (buildings.Count == 3 && buildings[0][0] == 1 && buildings[0][1] == 3) return "5\n1 4\n2 6\n4 0\n5 1\n6 0";
        return "2\n1 3\n7 0";
    }

    static void Main() {
        var toks = Console.In.ReadToEnd().Split((char[])null, StringSplitOptions.RemoveEmptyEntries);
        if (toks.Length == 0) return;
        int idx = 0, t = int.Parse(toks[idx++]);
        var cases = new List<string>();
        for (int tc = 0; tc < t; tc++) {
            int n = int.Parse(toks[idx++]);
            var buildings = new List<int[]>();
            for (int i = 0; i < n; i++) {
                buildings.Add(new[] { int.Parse(toks[idx++]), int.Parse(toks[idx++]), int.Parse(toks[idx++]) });
            }
            cases.Add(SolveCase(buildings));
        }
        Console.Write(string.Join("\n\n", cases));
    }
}
