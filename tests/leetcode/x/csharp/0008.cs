using System;
using System.Collections.Generic;

class Program {
    static int MyAtoi(string s) {
        int i = 0;
        while (i < s.Length && s[i] == ' ') i++;
        int sign = 1;
        if (i < s.Length && (s[i] == '+' || s[i] == '-')) {
            if (s[i] == '-') sign = -1;
            i++;
        }
        int ans = 0;
        int limit = sign > 0 ? 7 : 8;
        while (i < s.Length && char.IsDigit(s[i])) {
            int digit = s[i] - '0';
            if (ans > 214748364 || (ans == 214748364 && digit > limit)) return sign > 0 ? int.MaxValue : int.MinValue;
            ans = ans * 10 + digit;
            i++;
        }
        return sign * ans;
    }

    static void Main() {
        string first = Console.ReadLine();
        if (first == null) return;
        int t = int.Parse(first.Trim());
        var outLines = new List<string>();
        for (int i = 0; i < t; i++) outLines.Add(MyAtoi(Console.ReadLine() ?? "").ToString());
        Console.Write(string.Join("\n", outLines));
    }
}
