using System;
using System.Linq;

class Program {
    static int Solve(int[] a) {
        int best = 0;
        for (int i = 0; i < a.Length; i++) {
            int mn = a[i];
            for (int j = i; j < a.Length; j++) {
                if (a[j] < mn) mn = a[j];
                int area = mn * (j - i + 1);
                if (area > best) best = area;
            }
        }
        return best;
    }

    static void Main() {
        var data = Console.In.ReadToEnd().Split((char[])null, StringSplitOptions.RemoveEmptyEntries);
        if (data.Length == 0) return;
        int idx = 0, t = int.Parse(data[idx++]);
        var outLines = new string[t];
        for (int tc = 0; tc < t; tc++) {
            int n = int.Parse(data[idx++]);
            var a = new int[n];
            for (int i = 0; i < n; i++) a[i] = int.Parse(data[idx++]);
            outLines[tc] = Solve(a).ToString();
        }
        Console.Write(string.Join("\n", outLines));
    }
}
