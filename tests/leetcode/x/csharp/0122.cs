using System;
using System.Collections.Generic;

class Program {
    static int MaxProfit(int[] prices) {
        int best = 0;
        for (int i = 1; i < prices.Length; i++) {
            if (prices[i] > prices[i - 1]) best += prices[i] - prices[i - 1];
        }
        return best;
    }

    static void Main() {
        var lines = new List<string>();
        string line;
        while ((line = Console.ReadLine()) != null) lines.Add(line);
        if (lines.Count == 0) return;
        int t = int.Parse(lines[0].Trim());
        int idx = 1;
        var outLines = new List<string>();
        for (int tc = 0; tc < t; tc++) {
            int n = int.Parse(lines[idx++].Trim());
            var prices = new int[n];
            for (int i = 0; i < n; i++) prices[i] = int.Parse(lines[idx++].Trim());
            outLines.Add(MaxProfit(prices).ToString());
        }
        Console.Write(string.Join("\n", outLines));
    }
}
