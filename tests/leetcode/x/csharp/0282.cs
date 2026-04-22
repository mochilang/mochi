using System;
using System.Collections.Generic;

public class Program
{
    static void Dfs(string num, long target, int i, string expr, long value, long last, List<string> ans)
    {
        if (i == num.Length)
        {
            if (value == target) ans.Add(expr);
            return;
        }
        for (int j = i; j < num.Length; j++)
        {
            if (j > i && num[i] == '0') break;
            string s = num.Substring(i, j - i + 1);
            long n = long.Parse(s);
            if (i == 0)
            {
                Dfs(num, target, j + 1, s, n, n, ans);
            }
            else
            {
                Dfs(num, target, j + 1, expr + "+" + s, value + n, n, ans);
                Dfs(num, target, j + 1, expr + "-" + s, value - n, -n, ans);
                Dfs(num, target, j + 1, expr + "*" + s, value - last + last * n, last * n, ans);
            }
        }
    }

    static List<string> Solve(string num, long target)
    {
        var ans = new List<string>();
        Dfs(num, target, 0, "", 0, 0, ans);
        ans.Sort(StringComparer.Ordinal);
        return ans;
    }

    public static void Main()
    {
        string first = Console.ReadLine();
        if (first == null) return;
        int t = int.Parse(first.Trim());
        var blocks = new List<string>();
        for (int tc = 0; tc < t; tc++)
        {
            string num = (Console.ReadLine() ?? "").Trim();
            long target = long.Parse((Console.ReadLine() ?? "0").Trim());
            var ans = Solve(num, target);
            var lines = new List<string> { ans.Count.ToString() };
            lines.AddRange(ans);
            blocks.Add(string.Join("\n", lines));
        }
        Console.Write(string.Join("\n\n", blocks));
    }
}
