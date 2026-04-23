using System;
using System.Collections.Generic;

public class Program
{
    static bool IsSelfCrossing(int[] x)
    {
        for (int i = 3; i < x.Length; i++)
        {
            if (x[i] >= x[i - 2] && x[i - 1] <= x[i - 3]) return true;
            if (i >= 4 && x[i - 1] == x[i - 3] && x[i] + x[i - 4] >= x[i - 2]) return true;
            if (i >= 5 && x[i - 2] >= x[i - 4] && x[i] + x[i - 4] >= x[i - 2] && x[i - 1] <= x[i - 3] && x[i - 1] + x[i - 5] >= x[i - 3]) return true;
        }
        return false;
    }
    public static void Main() { string[] data = Console.In.ReadToEnd().Split((char[])null, StringSplitOptions.RemoveEmptyEntries); if (data.Length == 0) return; int idx = 0, t = int.Parse(data[idx++]); var outv = new List<string>(); for (int tc = 0; tc < t; tc++) { int n = int.Parse(data[idx++]); int[] x = new int[n]; for (int i = 0; i < n; i++) x[i] = int.Parse(data[idx++]); outv.Add(IsSelfCrossing(x) ? "true" : "false"); } Console.Write(string.Join("\n\n", outv)); }
}
