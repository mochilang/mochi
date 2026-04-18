using System;
using System.Collections.Generic;

class Program
{
    static List<List<string>> Solve(int n)
    {
        var cols = new bool[n];
        var d1 = new bool[2 * n];
        var d2 = new bool[2 * n];
        var board = new char[n][];
        for (int i = 0; i < n; i++) board[i] = new string('.', n).ToCharArray();
        var res = new List<List<string>>();
        Dfs(0, n, cols, d1, d2, board, res);
        return res;
    }

    static void Dfs(int r, int n, bool[] cols, bool[] d1, bool[] d2, char[][] board, List<List<string>> res)
    {
        if (r == n)
        {
            var sol = new List<string>();
            for (int i = 0; i < n; i++) sol.Add(new string(board[i]));
            res.Add(sol);
            return;
        }
        for (int c = 0; c < n; c++)
        {
            int a = r + c, b = r - c + n - 1;
            if (cols[c] || d1[a] || d2[b]) continue;
            cols[c] = d1[a] = d2[b] = true;
            board[r][c] = 'Q';
            Dfs(r + 1, n, cols, d1, d2, board, res);
            board[r][c] = '.';
            cols[c] = d1[a] = d2[b] = false;
        }
    }

    static void Main()
    {
        var lines = Console.In.ReadToEnd().Split(new[] { "\r\n", "\n" }, StringSplitOptions.None);
        if (lines.Length == 0 || lines[0].Trim() == "") return;
        int idx = 0, t = int.Parse(lines[idx++].Trim());
        var outp = new List<string>();
        for (int tc = 0; tc < t; tc++)
        {
            int n = int.Parse(lines[idx++].Trim());
            var sols = Solve(n);
            outp.Add(sols.Count.ToString());
        }
        Console.Write(string.Join("\n", outp));
    }
}
