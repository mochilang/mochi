using System;
using System.Collections.Generic;

public class Program
{
    static int Solve(int[][] costs)
    {
        if (costs.Length == 0) return 0;
        var prev = (int[])costs[0].Clone();
        for (int r = 1; r < costs.Length; r++)
        {
            int min1 = int.MaxValue, min2 = int.MaxValue, idx1 = -1;
            for (int i = 0; i < prev.Length; i++)
            {
                if (prev[i] < min1)
                {
                    min2 = min1;
                    min1 = prev[i];
                    idx1 = i;
                }
                else if (prev[i] < min2)
                {
                    min2 = prev[i];
                }
            }
            var cur = new int[prev.Length];
            for (int i = 0; i < prev.Length; i++)
            {
                cur[i] = costs[r][i] + (i == idx1 ? min2 : min1);
            }
            prev = cur;
        }
        int ans = prev[0];
        foreach (int v in prev) if (v < ans) ans = v;
        return ans;
    }

    public static void Main()
    {
        var data = Console.In.ReadToEnd().Split((char[])null, StringSplitOptions.RemoveEmptyEntries);
        if (data.Length == 0) return;
        int idx = 0;
        int t = int.Parse(data[idx++]);
        var outLines = new List<string>();
        for (int tc = 0; tc < t; tc++)
        {
            int n = int.Parse(data[idx++]);
            int k = int.Parse(data[idx++]);
            var costs = new int[n][];
            for (int i = 0; i < n; i++)
            {
                costs[i] = new int[k];
                for (int j = 0; j < k; j++) costs[i][j] = int.Parse(data[idx++]);
            }
            outLines.Add(Solve(costs).ToString());
        }
        Console.Write(string.Join("\n", outLines));
    }
}
