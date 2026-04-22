using System;
using System.Collections.Generic;

public class Program
{
    static int MinTotalDistance(int[,] grid, int rowsCount, int colsCount)
    {
        var rows = new List<int>();
        var cols = new List<int>();
        for (int i = 0; i < rowsCount; i++)
        {
            for (int j = 0; j < colsCount; j++)
            {
                if (grid[i, j] == 1) rows.Add(i);
            }
        }
        for (int j = 0; j < colsCount; j++)
        {
            for (int i = 0; i < rowsCount; i++)
            {
                if (grid[i, j] == 1) cols.Add(j);
            }
        }
        int mr = rows[rows.Count / 2];
        int mc = cols[cols.Count / 2];
        int ans = 0;
        foreach (int r in rows) ans += Math.Abs(r - mr);
        foreach (int c in cols) ans += Math.Abs(c - mc);
        return ans;
    }

    public static void Main()
    {
        var data = Console.In.ReadToEnd().Split((char[])null, StringSplitOptions.RemoveEmptyEntries);
        if (data.Length == 0) return;
        int idx = 0;
        int t = int.Parse(data[idx++]);
        var blocks = new List<string>();
        for (int tc = 0; tc < t; tc++)
        {
            int r = int.Parse(data[idx++]);
            int c = int.Parse(data[idx++]);
            var grid = new int[r, c];
            for (int i = 0; i < r; i++)
            {
                for (int j = 0; j < c; j++)
                {
                    grid[i, j] = int.Parse(data[idx++]);
                }
            }
            blocks.Add(MinTotalDistance(grid, r, c).ToString());
        }
        Console.Write(string.Join("\n\n", blocks));
    }
}
