using System;
using System.Collections.Generic;

class Program {
    class Node {
        public Dictionary<char, Node> Children = new Dictionary<char, Node>();
        public string Word = null;
    }

    static void Dfs(char[][] board, int rows, int cols, int r, int c, Node node, List<string> found) {
        char ch = board[r][c];
        if (!node.Children.TryGetValue(ch, out var next)) return;
        if (next.Word != null) {
            found.Add(next.Word);
            next.Word = null;
        }
        board[r][c] = '#';
        if (r > 0 && board[r - 1][c] != '#') Dfs(board, rows, cols, r - 1, c, next, found);
        if (r + 1 < rows && board[r + 1][c] != '#') Dfs(board, rows, cols, r + 1, c, next, found);
        if (c > 0 && board[r][c - 1] != '#') Dfs(board, rows, cols, r, c - 1, next, found);
        if (c + 1 < cols && board[r][c + 1] != '#') Dfs(board, rows, cols, r, c + 1, next, found);
        board[r][c] = ch;
    }

    static List<string> Solve(char[][] board, string[] words) {
        var root = new Node();
        foreach (var word in words) {
            var node = root;
            foreach (char ch in word) {
                if (!node.Children.ContainsKey(ch)) node.Children[ch] = new Node();
                node = node.Children[ch];
            }
            node.Word = word;
        }
        int rows = board.Length, cols = board[0].Length;
        var found = new List<string>();
        for (int r = 0; r < rows; r++)
            for (int c = 0; c < cols; c++)
                if (root.Children.ContainsKey(board[r][c])) Dfs(board, rows, cols, r, c, root, found);
        found.Sort(StringComparer.Ordinal);
        return found;
    }

    static void Main() {
        var toks = Console.In.ReadToEnd().Split((char[])null, StringSplitOptions.RemoveEmptyEntries);
        if (toks.Length == 0) return;
        int idx = 0, t = int.Parse(toks[idx++]);
        var cases = new List<string>();
        for (int tc = 0; tc < t; tc++) {
            int rows = int.Parse(toks[idx++]);
            int cols = int.Parse(toks[idx++]);
            var board = new char[rows][];
            for (int i = 0; i < rows; i++) board[i] = toks[idx++].ToCharArray();
            int n = int.Parse(toks[idx++]);
            var words = new string[n];
            for (int i = 0; i < n; i++) words[i] = toks[idx++];
            var ans = Solve(board, words);
            var lines = new List<string> { ans.Count.ToString() };
            lines.AddRange(ans);
            cases.Add(string.Join("\n", lines));
            _ = cols;
        }
        Console.Write(string.Join("\n\n", cases));
    }
}
