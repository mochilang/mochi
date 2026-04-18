using System;
using System.Collections.Generic;

class Program
{
    static bool Valid(char[][] b, int r, int c, char ch)
    {
        for (int i = 0; i < 9; i++)
        {
            if (b[r][i] == ch || b[i][c] == ch) return false;
        }
        int br = (r / 3) * 3;
        int bc = (c / 3) * 3;
        for (int i = br; i < br + 3; i++)
        {
            for (int j = bc; j < bc + 3; j++)
            {
                if (b[i][j] == ch) return false;
            }
        }
        return true;
    }

    static bool Solve(char[][] b)
    {
        for (int r = 0; r < 9; r++)
        {
            for (int c = 0; c < 9; c++)
            {
                if (b[r][c] != '.') continue;
                for (char ch = '1'; ch <= '9'; ch++)
                {
                    if (!Valid(b, r, c, ch)) continue;
                    b[r][c] = ch;
                    if (Solve(b)) return true;
                    b[r][c] = '.';
                }
                return false;
            }
        }
        return true;
    }

    static void Main()
    {
        var lines = Console.In.ReadToEnd().Split(new[] { "\r\n", "\n" }, StringSplitOptions.None);
        if (lines.Length == 0 || lines[0].Trim() == "") return;
        int idx = 0;
        int t = int.Parse(lines[idx++].Trim());
        var output = new List<string>();
        for (int tc = 0; tc < t; tc++)
        {
            var b = new char[9][];
            for (int i = 0; i < 9; i++)
            {
                b[i] = (idx < lines.Length ? lines[idx++] : "").ToCharArray();
            }
            Solve(b);
            for (int i = 0; i < 9; i++)
            {
                output.Add(new string(b[i]));
            }
        }
        Console.Write(string.Join("\n", output));
    }
}
