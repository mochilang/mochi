using System;
using System.Collections.Generic;

public class Program
{
    static int Find(int x, Dictionary<int, int> parent)
    {
        while (parent[x] != x)
        {
            parent[x] = parent[parent[x]];
            x = parent[x];
        }
        return x;
    }

    static bool Union(int a, int b, Dictionary<int, int> parent, Dictionary<int, int> rank)
    {
        int ra = Find(a, parent), rb = Find(b, parent);
        if (ra == rb) return false;
        if (rank[ra] < rank[rb])
        {
            int t = ra; ra = rb; rb = t;
        }
        parent[rb] = ra;
        if (rank[ra] == rank[rb]) rank[ra]++;
        return true;
    }

    static List<int> Solve(int m, int n, List<int[]> positions)
    {
        var parent = new Dictionary<int, int>();
        var rank = new Dictionary<int, int>();
        var ans = new List<int>();
        int count = 0;
        int[][] dirs = { new[] { 1, 0 }, new[] { -1, 0 }, new[] { 0, 1 }, new[] { 0, -1 } };
        foreach (var p in positions)
        {
            int r = p[0], c = p[1];
            int idx = r * n + c;
            if (parent.ContainsKey(idx))
            {
                ans.Add(count);
                continue;
            }
            parent[idx] = idx;
            rank[idx] = 0;
            count++;
            foreach (var d in dirs)
            {
                int nr = r + d[0], nc = c + d[1];
                if (nr >= 0 && nr < m && nc >= 0 && nc < n)
                {
                    int nei = nr * n + nc;
                    if (parent.ContainsKey(nei) && Union(idx, nei, parent, rank)) count--;
                }
            }
            ans.Add(count);
        }
        return ans;
    }

    static string FmtList(List<int> a) => "[" + string.Join(",", a) + "]";

    public static void Main()
    {
        string[] data = Console.In.ReadToEnd().Split((char[])null, StringSplitOptions.RemoveEmptyEntries);
        if (data.Length == 0) return;
        int idx = 0;
        int t = int.Parse(data[idx++]);
        var blocks = new List<string>();
        for (int tc = 0; tc < t; tc++)
        {
            int m = int.Parse(data[idx++]);
            int n = int.Parse(data[idx++]);
            int k = int.Parse(data[idx++]);
            var positions = new List<int[]>();
            for (int i = 0; i < k; i++)
            {
                positions.Add(new[] { int.Parse(data[idx++]), int.Parse(data[idx++]) });
            }
            blocks.Add(FmtList(Solve(m, n, positions)));
        }
        Console.Write(string.Join("\n\n", blocks));
    }
}
