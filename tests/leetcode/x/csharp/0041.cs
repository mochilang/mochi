using System;
using System.Collections.Generic;

class Program
{
    static int FirstMissingPositive(int[] nums)
    {
        int n = nums.Length;
        int i = 0;
        while (i < n)
        {
            int v = nums[i];
            if (v >= 1 && v <= n && nums[v - 1] != v)
            {
                int tmp = nums[i];
                nums[i] = nums[v - 1];
                nums[v - 1] = tmp;
            }
            else
            {
                i++;
            }
        }
        for (i = 0; i < n; i++) if (nums[i] != i + 1) return i + 1;
        return n + 1;
    }

    static void Main()
    {
        var lines = Console.In.ReadToEnd().Split(new[] {"\r\n", "\n"}, StringSplitOptions.None);
        if (lines.Length == 0 || lines[0].Trim() == "") return;
        int idx = 0;
        int t = int.Parse(lines[idx++].Trim());
        var output = new List<string>();
        for (int tc = 0; tc < t; tc++)
        {
            int n = int.Parse(lines[idx++].Trim());
            var nums = new int[n];
            for (int i = 0; i < n; i++) nums[i] = int.Parse(lines[idx++].Trim());
            output.Add(FirstMissingPositive(nums).ToString());
        }
        Console.Write(string.Join("\n", output));
    }
}
