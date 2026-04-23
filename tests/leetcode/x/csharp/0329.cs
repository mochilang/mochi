using System;
using System.Collections.Generic;

public class Program
{
    static int[,] matrix;
    static int[,] memo;
    static int rows, cols;
    static int[] dirs = { 1, 0, -1, 0, 1 };

    static int Dfs(int r, int c)
    {
        if (memo[r, c] != 0) return memo[r, c];
        int best = 1;
        for (int k = 0; k < 4; k++)
        {
            int nr = r + dirs[k], nc = c + dirs[k + 1];
            if (nr >= 0 && nr < rows && nc >= 0 && nc < cols && matrix[nr, nc] > matrix[r, c])
                best = Math.Max(best, 1 + Dfs(nr, nc));
        }
        memo[r, c] = best;
        return best;
    }

    static int LongestIncreasingPath(int[,] m)
    {
        matrix = m; rows = m.GetLength(0); cols = m.GetLength(1); memo = new int[rows, cols];
        int ans = 0;
        for (int r = 0; r < rows; r++) for (int c = 0; c < cols; c++) ans = Math.Max(ans, Dfs(r, c));
        return ans;
    }

    public static void Main()
    {
        string[] data = Console.In.ReadToEnd().Split((char[])null, StringSplitOptions.RemoveEmptyEntries);
        if (data.Length == 0) return;
        int idx = 0, t = int.Parse(data[idx++]);
        var outv = new List<string>();
        for (int tc = 0; tc < t; tc++)
        {
            int r = int.Parse(data[idx++]), c = int.Parse(data[idx++]);
            int[,] m = new int[r, c];
            for (int i = 0; i < r; i++) for (int j = 0; j < c; j++) m[i, j] = int.Parse(data[idx++]);
            outv.Add(LongestIncreasingPath(m).ToString());
        }
        Console.Write(string.Join("\n\n", outv));
    }
}
