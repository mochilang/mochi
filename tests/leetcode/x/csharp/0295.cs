using System;
using System.Collections.Generic;
using System.Globalization;

public class MedianFinder
{
    private readonly List<int> data = new List<int>();

    public void AddNum(int num)
    {
        int pos = data.BinarySearch(num);
        if (pos < 0) pos = ~pos;
        data.Insert(pos, num);
    }

    public double FindMedian()
    {
        int n = data.Count;
        if ((n & 1) == 1) return data[n / 2];
        return (data[n / 2 - 1] + data[n / 2]) / 2.0;
    }
}

public class Program
{
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
            int m = int.Parse(lines[idx++]);
            var mf = new MedianFinder();
            var outLines = new List<string>();
            for (int i = 0; i < m; i++)
            {
                string[] parts = lines[idx++].Split(' ');
                if (parts[0] == "addNum")
                {
                    mf.AddNum(int.Parse(parts[1]));
                }
                else
                {
                    outLines.Add(mf.FindMedian().ToString("0.0", CultureInfo.InvariantCulture));
                }
            }
            blocks.Add(string.Join("\n", outLines));
        }
        Console.Write(string.Join("\n\n", blocks));
    }
}
