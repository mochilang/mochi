using System;
using System.Collections.Generic;

class Program {
    static bool IsNumber(string s) {
        bool seenDigit = false, seenDot = false, seenExp = false, digitAfterExp = true;
        for (int i = 0; i < s.Length; i++) {
            char ch = s[i];
            if (ch >= '0' && ch <= '9') {
                seenDigit = true;
                if (seenExp) digitAfterExp = true;
            } else if (ch == '+' || ch == '-') {
                if (i != 0 && s[i - 1] != 'e' && s[i - 1] != 'E') return false;
            } else if (ch == '.') {
                if (seenDot || seenExp) return false;
                seenDot = true;
            } else if (ch == 'e' || ch == 'E') {
                if (seenExp || !seenDigit) return false;
                seenExp = true;
                digitAfterExp = false;
            } else return false;
        }
        return seenDigit && digitAfterExp;
    }

    static void Main() {
        var lines = Console.In.ReadToEnd().Split(new[] { "\r\n", "\n" }, StringSplitOptions.None);
        if (lines.Length == 0 || lines[0].Trim() == "") return;
        int t = int.Parse(lines[0].Trim());
        var outp = new List<string>();
        for (int i = 0; i < t; i++) outp.Add(IsNumber(lines[i + 1]) ? "true" : "false");
        Console.Write(string.Join("\n", outp));
    }
}
