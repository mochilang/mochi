using System;
using System.Collections.Generic;
using System.Text;

class Program {
    static string ConvertZigzag(string s, int numRows) {
        if (numRows <= 1 || numRows >= s.Length) return s;
        int cycle = 2 * numRows - 2;
        var sb = new StringBuilder();
        for (int row = 0; row < numRows; row++) {
            for (int i = row; i < s.Length; i += cycle) {
                sb.Append(s[i]);
                int diag = i + cycle - 2 * row;
                if (row > 0 && row < numRows - 1 && diag < s.Length) sb.Append(s[diag]);
            }
        }
        return sb.ToString();
    }

    static void Main() {
        string first = Console.ReadLine();
        if (first == null) return;
        int t = int.Parse(first.Trim());
        var outLines = new List<string>();
        for (int i = 0; i < t; i++) {
            string s = Console.ReadLine() ?? "";
            string rowLine = Console.ReadLine() ?? "1";
            outLines.Add(ConvertZigzag(s, int.Parse(rowLine.Trim())));
        }
        Console.Write(string.Join("\n", outLines));
    }
}
