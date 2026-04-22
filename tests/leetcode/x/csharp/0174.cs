using System;

class Program {
    static int Solve(int[][] dungeon) {
        int cols = dungeon[0].Length;
        long inf = (long)4e18;
        var dp = new long[cols + 1];
        for (int i = 0; i <= cols; i++) dp[i] = inf;
        dp[cols - 1] = 1;
        for (int i = dungeon.Length - 1; i >= 0; i--) {
            for (int j = cols - 1; j >= 0; j--) {
                long need = Math.Min(dp[j], dp[j + 1]) - dungeon[i][j];
                dp[j] = need <= 1 ? 1 : need;
            }
        }
        return (int)dp[0];
    }

    static void Main() {
        var toks = Console.In.ReadToEnd().Split((char[])null, StringSplitOptions.RemoveEmptyEntries);
        if (toks.Length == 0) return;
        int idx = 0, t = int.Parse(toks[idx++]);
        var outLines = new string[t];
        for (int tc = 0; tc < t; tc++) {
            int rows = int.Parse(toks[idx++]);
            int cols = int.Parse(toks[idx++]);
            var dungeon = new int[rows][];
            for (int i = 0; i < rows; i++) {
                dungeon[i] = new int[cols];
                for (int j = 0; j < cols; j++) dungeon[i][j] = int.Parse(toks[idx++]);
            }
            outLines[tc] = Solve(dungeon).ToString();
        }
        Console.Write(string.Join("\n", outLines));
    }
}
