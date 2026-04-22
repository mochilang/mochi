using System;

class Program {
    static int Solve(int k, int[] prices) {
        int n = prices.Length;
        if (k >= n / 2) {
            int best = 0;
            for (int i = 1; i < n; i++) if (prices[i] > prices[i - 1]) best += prices[i] - prices[i - 1];
            return best;
        }
        long negInf = -(1L << 60);
        var buy = new long[k + 1];
        var sell = new long[k + 1];
        for (int i = 1; i <= k; i++) buy[i] = negInf;
        foreach (int price in prices) {
            for (int t = 1; t <= k; t++) {
                buy[t] = Math.Max(buy[t], sell[t - 1] - price);
                sell[t] = Math.Max(sell[t], buy[t] + price);
            }
        }
        return (int)sell[k];
    }

    static void Main() {
        var toks = Console.In.ReadToEnd().Split((char[])null, StringSplitOptions.RemoveEmptyEntries);
        if (toks.Length == 0) return;
        int idx = 0, t = int.Parse(toks[idx++]);
        var outLines = new string[t];
        for (int tc = 0; tc < t; tc++) {
            int k = int.Parse(toks[idx++]);
            int n = int.Parse(toks[idx++]);
            var prices = new int[n];
            for (int i = 0; i < n; i++) prices[i] = int.Parse(toks[idx++]);
            outLines[tc] = Solve(k, prices).ToString();
        }
        Console.Write(string.Join("\n", outLines));
    }
}
