using System;
using System.Collections.Generic;

class Program {
    static int Candy(List<int> ratings) {
        int n = ratings.Count;
        var candies = new int[n];
        for (int i = 0; i < n; i++) candies[i] = 1;
        for (int i = 1; i < n; i++) {
            if (ratings[i] > ratings[i - 1]) candies[i] = candies[i - 1] + 1;
        }
        for (int i = n - 2; i >= 0; i--) {
            if (ratings[i] > ratings[i + 1]) candies[i] = Math.Max(candies[i], candies[i + 1] + 1);
        }
        int total = 0;
        foreach (int v in candies) total += v;
        return total;
    }

    static void Main() {
        var lines = new List<string>();
        string line;
        while ((line = Console.ReadLine()) != null) lines.Add(line);
        if (lines.Count == 0) return;
        int tc = int.Parse(lines[0]);
        int idx = 1;
        var outp = new List<string>();
        for (int t = 0; t < tc; t++) {
            int n = int.Parse(lines[idx++]);
            var ratings = new List<int>();
            for (int i = 0; i < n; i++) ratings.Add(int.Parse(lines[idx++]));
            outp.Add(Candy(ratings).ToString());
        }
        Console.Write(string.Join("\n\n", outp));
    }
}
