using System;
using System.Collections.Generic;

public class Program
{
    static int MaxCoins(int[] nums)
    {
        int[] vals = new int[nums.Length + 2];
        vals[0] = 1;
        vals[vals.Length - 1] = 1;
        for (int i = 0; i < nums.Length; i++) vals[i + 1] = nums[i];
        int n = vals.Length;
        int[,] dp = new int[n, n];
        for (int length = 2; length < n; length++)
        {
            for (int left = 0; left + length < n; left++)
            {
                int right = left + length;
                for (int k = left + 1; k < right; k++)
                {
                    int cand = dp[left, k] + dp[k, right] + vals[left] * vals[k] * vals[right];
                    if (cand > dp[left, right]) dp[left, right] = cand;
                }
            }
        }
        return dp[0, n - 1];
    }

    public static void Main()
    {
        string[] data = Console.In.ReadToEnd().Split((char[])null, StringSplitOptions.RemoveEmptyEntries);
        if (data.Length == 0) return;
        int idx = 0;
        int t = int.Parse(data[idx++]);
        var blocks = new List<string>();
        for (int tc = 0; tc < t; tc++)
        {
            int n = int.Parse(data[idx++]);
            int[] nums = new int[n];
            for (int i = 0; i < n; i++) nums[i] = int.Parse(data[idx++]);
            blocks.Add(MaxCoins(nums).ToString());
        }
        Console.Write(string.Join("\n\n", blocks));
    }
}
