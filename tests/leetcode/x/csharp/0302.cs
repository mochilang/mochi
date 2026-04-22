using System;
using System.Collections.Generic;

public class Program
{
    static int MinArea(List<string> image, int x, int y)
    {
        int top = image.Count, bottom = -1;
        int left = image[0].Length, right = -1;
        for (int i = 0; i < image.Count; i++)
        {
            for (int j = 0; j < image[i].Length; j++)
            {
                if (image[i][j] == '1')
                {
                    if (i < top) top = i;
                    if (i > bottom) bottom = i;
                    if (j < left) left = j;
                    if (j > right) right = j;
                }
            }
        }
        return (bottom - top + 1) * (right - left + 1);
    }

    public static void Main()
    {
        var lines = new List<string>();
        string line;
        while ((line = Console.ReadLine()) != null)
        {
            line = line.Trim();
            if (line.Length > 0) lines.Add(line);
        }
        if (lines.Count == 0) return;
        int t = int.Parse(lines[0]);
        int idx = 1;
        var blocks = new List<string>();
        for (int tc = 0; tc < t; tc++)
        {
            string[] parts = lines[idx++].Split(' ');
            int r = int.Parse(parts[0]);
            var image = new List<string>();
            for (int i = 0; i < r; i++) image.Add(lines[idx++]);
            parts = lines[idx++].Split(' ');
            int x = int.Parse(parts[0]);
            int y = int.Parse(parts[1]);
            blocks.Add(MinArea(image, x, y).ToString());
        }
        Console.Write(string.Join("\n\n", blocks));
    }
}
