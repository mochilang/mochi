using System;
using System.Collections.Generic;

class Program {
    static int MaxProfit(int[] prices) {
        int buy1 = -1000000000, sell1 = 0, buy2 = -1000000000, sell2 = 0;
        foreach (int p in prices) {
            buy1 = Math.Max(buy1, -p);
            sell1 = Math.Max(sell1, buy1 + p);
            buy2 = Math.Max(buy2, sell1 - p);
            sell2 = Math.Max(sell2, buy2 + p);
        }
        return sell2;
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
