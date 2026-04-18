using System;
using System.Collections.Generic;

class Program {
    static (int, int) Expand(string s, int left, int right) {
        while (left >= 0 && right < s.Length && s[left] == s[right]) {
            left--;
            right++;
        }
        return (left + 1, right - left - 1);
    }

    static string LongestPalindrome(string s) {
        int bestStart = 0;
        int bestLen = s.Length > 0 ? 1 : 0;
        for (int i = 0; i < s.Length; i++) {
            var odd = Expand(s, i, i);
            if (odd.Item2 > bestLen) {
                bestStart = odd.Item1;
                bestLen = odd.Item2;
            }
            var even = Expand(s, i, i + 1);
            if (even.Item2 > bestLen) {
                bestStart = even.Item1;
                bestLen = even.Item2;
            }
        }
        return s.Substring(bestStart, bestLen);
    }

    static void Main() {
        string first = Console.ReadLine();
        if (first == null) return;
        int t = int.Parse(first.Trim());
        var outLines = new List<string>();
        for (int i = 0; i < t; i++) {
            string s = Console.ReadLine() ?? "";
            outLines.Add(LongestPalindrome(s));
        }
        Console.Write(string.Join("\n", outLines));
    }
}
