using System;
using System.Collections.Generic;

public class Program
{
    static int[] Solve(int[] values, double target, int k)
    {
        int right = 0;
        while (right < values.Length && values[right] < target) right++;
        int left = right - 1;
        var ans = new int[k];
        for (int i = 0; i < k; i++)
        {
            if (left < 0) ans[i] = values[right++];
            else if (right >= values.Length) ans[i] = values[left--];
            else if (Math.Abs(values[left] - target) <= Math.Abs(values[right] - target)) ans[i] = values[left--];
            else ans[i] = values[right++];
        }
        return ans;
    }

    public static void Main()
    {
        var data = Console.In.ReadToEnd().Split((char[])null, StringSplitOptions.RemoveEmptyEntries);
        if (data.Length == 0) return;
        int idx = 0;
        int t = int.Parse(data[idx++]);
        var blocks = new List<string>();
        for (int tc = 0; tc < t; tc++)
        {
            int n = int.Parse(data[idx++]);
            var values = new int[n];
            for (int i = 0; i < n; i++) values[i] = int.Parse(data[idx++]);
            double target = double.Parse(data[idx++], System.Globalization.CultureInfo.InvariantCulture);
            int k = int.Parse(data[idx++]);
            var ans = Solve(values, target, k);
            var lines = new List<string> { ans.Length.ToString() };
            foreach (int x in ans) lines.Add(x.ToString());
            blocks.Add(string.Join("\n", lines));
        }
        Console.Write(string.Join("\n\n", blocks));
    }
}
