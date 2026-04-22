using System;
using System.Collections.Generic;

class Program {
    static int Gcd(int a, int b) {
        while (b != 0) {
            int t = a % b;
            a = b;
            b = t;
        }
        return Math.Abs(a);
    }

    static int MaxPoints(List<Tuple<int, int>> points) {
        int n = points.Count;
        if (n <= 2) return n;
        int best = 0;
        for (int i = 0; i < n; i++) {
            var slopes = new Dictionary<string, int>();
            int local = 0;
            for (int j = i + 1; j < n; j++) {
                int dx = points[j].Item1 - points[i].Item1;
                int dy = points[j].Item2 - points[i].Item2;
                int g = Gcd(dx, dy);
                dx /= g;
                dy /= g;
                if (dx < 0) {
                    dx = -dx;
                    dy = -dy;
                } else if (dx == 0) {
                    dy = 1;
                } else if (dy == 0) {
                    dx = 1;
                }
                string key = dy + "/" + dx;
                if (!slopes.ContainsKey(key)) slopes[key] = 0;
                slopes[key]++;
                local = Math.Max(local, slopes[key]);
            }
            best = Math.Max(best, local + 1);
        }
        return best;
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
            int n = int.Parse(lines[idx++]);
            var points = new List<Tuple<int, int>>();
            for (int i = 0; i < n; i++) {
                string[] parts = lines[idx++].Split();
                points.Add(Tuple.Create(int.Parse(parts[0]), int.Parse(parts[1])));
            }
            outp.Add(MaxPoints(points).ToString());
        }
        Console.Write(string.Join("\n\n", outp));
    }
}
