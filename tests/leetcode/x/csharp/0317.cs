using System;
using System.Collections.Generic;

public class Program
{
    static int ShortestDistance(int[,] grid, int rows, int cols)
    {
        int[,] dist = new int[rows, cols];
        int[,] reach = new int[rows, cols];
        int buildings = 0;
        for (int sr = 0; sr < rows; sr++)
        {
            for (int sc = 0; sc < cols; sc++)
            {
                if (grid[sr, sc] != 1) continue;
                buildings++;
                bool[,] seen = new bool[rows, cols];
                var q = new Queue<int[]>();
                q.Enqueue(new[] { sr, sc, 0 });
                seen[sr, sc] = true;
                while (q.Count > 0)
                {
                    int[] cur = q.Dequeue();
                    int r = cur[0], c = cur[1], d = cur[2];
                    int[][] dirs = { new[] { 1, 0 }, new[] { -1, 0 }, new[] { 0, 1 }, new[] { 0, -1 } };
                    foreach (int[] dir in dirs)
                    {
                        int nr = r + dir[0], nc = c + dir[1];
                        if (nr >= 0 && nr < rows && nc >= 0 && nc < cols && !seen[nr, nc])
                        {
                            seen[nr, nc] = true;
                            if (grid[nr, nc] == 0)
                            {
                                dist[nr, nc] += d + 1;
                                reach[nr, nc] += 1;
                                q.Enqueue(new[] { nr, nc, d + 1 });
                            }
                        }
                    }
                }
            }
        }
        int ans = -1;
        for (int r = 0; r < rows; r++)
        {
            for (int c = 0; c < cols; c++)
            {
                if (grid[r, c] == 0 && reach[r, c] == buildings)
                {
                    if (ans == -1 || dist[r, c] < ans) ans = dist[r, c];
                }
            }
        }
        return ans;
    }

    public static void Main()
    {
        string[] data = Console.In.ReadToEnd().Split((char[])null, StringSplitOptions.RemoveEmptyEntries);
        if (data.Length == 0) return;
        int pos = 0;
        int t = int.Parse(data[pos++]);
        var blocks = new List<string>();
        for (int tc = 0; tc < t; tc++)
        {
            int rows = int.Parse(data[pos++]), cols = int.Parse(data[pos++]);
            int[,] grid = new int[rows, cols];
            for (int i = 0; i < rows; i++)
                for (int j = 0; j < cols; j++)
                    grid[i, j] = int.Parse(data[pos++]);
            blocks.Add(ShortestDistance(grid, rows, cols).ToString());
        }
        Console.Write(string.Join("\n\n", blocks));
    }
}
