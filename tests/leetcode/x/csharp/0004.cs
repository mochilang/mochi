using System;
using System.Collections.Generic;
using System.Globalization;

class Program {
    static double Median(int[] a, int[] b) {
        var m = new int[a.Length + b.Length];
        int i = 0, j = 0, k = 0;
        while (i < a.Length && j < b.Length) m[k++] = a[i] <= b[j] ? a[i++] : b[j++];
        while (i < a.Length) m[k++] = a[i++];
        while (j < b.Length) m[k++] = b[j++];
        if (k % 2 == 1) return m[k / 2];
        return (m[k / 2 - 1] + m[k / 2]) / 2.0;
    }

    static void Main() {
        var lines = new List<string>();
        string line; while ((line = Console.ReadLine()) != null) lines.Add(line);
        if (lines.Count == 0) return;
        int t = int.Parse(lines[0].Trim());
        int idx = 1;
        var outLines = new List<string>();
        for (int tc = 0; tc < t; tc++) {
            int n = int.Parse(lines[idx++].Trim());
            var a = new int[n]; for (int i=0;i<n;i++) a[i] = int.Parse(lines[idx++].Trim());
            int m = int.Parse(lines[idx++].Trim());
            var b = new int[m]; for (int i=0;i<m;i++) b[i] = int.Parse(lines[idx++].Trim());
            outLines.Add(Median(a,b).ToString("F1", CultureInfo.InvariantCulture));
        }
        Console.Write(string.Join("\n", outLines));
    }
}
