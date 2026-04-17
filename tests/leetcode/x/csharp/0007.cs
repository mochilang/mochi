using System;
using System.Collections.Generic;

class Program {
    static int ReverseInt(int x) {
        int ans = 0;
        while (x != 0) {
            int digit = x % 10;
            x /= 10;
            if (ans > int.MaxValue / 10 || (ans == int.MaxValue / 10 && digit > 7)) return 0;
            if (ans < int.MinValue / 10 || (ans == int.MinValue / 10 && digit < -8)) return 0;
            ans = ans * 10 + digit;
        }
        return ans;
    }

    static void Main() {
        string first = Console.ReadLine();
        if (first == null) return;
        int t = int.Parse(first.Trim());
        var outLines = new List<string>();
        for (int i = 0; i < t; i++) {
            string line = Console.ReadLine() ?? "0";
            outLines.Add(ReverseInt(int.Parse(line.Trim())).ToString());
        }
        Console.Write(string.Join("\n", outLines));
    }
}
