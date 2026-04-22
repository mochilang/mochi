using System;
using System.Collections.Generic;
using System.Text;

public class Program
{
    static void Dfs(string s, int i, int left, int right, int balance, StringBuilder path, SortedSet<string> ans)
    {
        if (i == s.Length)
        {
            if (left == 0 && right == 0 && balance == 0) ans.Add(path.ToString());
            return;
        }
        char ch = s[i];
        int len = path.Length;
        if (ch == '(')
        {
            if (left > 0) Dfs(s, i + 1, left - 1, right, balance, path, ans);
            path.Append(ch);
            Dfs(s, i + 1, left, right, balance + 1, path, ans);
            path.Length = len;
        }
        else if (ch == ')')
        {
            if (right > 0) Dfs(s, i + 1, left, right - 1, balance, path, ans);
            if (balance > 0)
            {
                path.Append(ch);
                Dfs(s, i + 1, left, right, balance - 1, path, ans);
                path.Length = len;
            }
        }
        else
        {
            path.Append(ch);
            Dfs(s, i + 1, left, right, balance, path, ans);
            path.Length = len;
        }
    }

    static List<string> Solve(string s)
    {
        int leftRemove = 0, rightRemove = 0;
        foreach (char ch in s)
        {
            if (ch == '(') leftRemove++;
            else if (ch == ')')
            {
                if (leftRemove > 0) leftRemove--;
                else rightRemove++;
            }
        }
        var ans = new SortedSet<string>();
        Dfs(s, 0, leftRemove, rightRemove, 0, new StringBuilder(), ans);
        return new List<string>(ans);
    }

    public static void Main()
    {
        string first = Console.ReadLine();
        if (string.IsNullOrEmpty(first)) return;
        int t = int.Parse(first.Trim());
        var blocks = new List<string>();
        for (int tc = 0; tc < t; tc++)
        {
            string s = Console.ReadLine() ?? "";
            List<string> ans = Solve(s);
            var lines = new List<string> { ans.Count.ToString() };
            lines.AddRange(ans);
            blocks.Add(string.Join("\n", lines));
        }
        Console.Write(string.Join("\n\n", blocks));
    }
}
