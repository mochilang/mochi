using System;
using System.Collections.Generic;
using System.Linq;

public class Program
{
    static int[] Solve(int[] nums, int k)
    {
        var dq = new LinkedList<int>();
        var ans = new List<int>();
        for (int i = 0; i < nums.Length; i++)
        {
            while (dq.Count > 0 && dq.First.Value <= i - k) dq.RemoveFirst();
            while (dq.Count > 0 && nums[dq.Last.Value] <= nums[i]) dq.RemoveLast();
            dq.AddLast(i);
            if (i >= k - 1) ans.Add(nums[dq.First.Value]);
        }
        return ans.ToArray();
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
            var nums = new int[n];
            for (int i = 0; i < n; i++) nums[i] = int.Parse(data[idx++]);
            int k = int.Parse(data[idx++]);
            var ans = Solve(nums, k);
            blocks.Add(string.Join("\n", new[] { ans.Length.ToString() }.Concat(Array.ConvertAll(ans, x => x.ToString()))));
        }
        Console.Write(string.Join("\n\n", blocks));
    }
}
