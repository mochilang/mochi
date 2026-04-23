using System;
using System.Collections.Generic;

public class Program
{
    static int Sort(long[] pref, long[] tmp, int lo, int hi, int lower, int upper)
    {
        if (hi - lo <= 1) return 0;
        int mid = (lo + hi) / 2;
        int ans = Sort(pref, tmp, lo, mid, lower, upper) + Sort(pref, tmp, mid, hi, lower, upper);
        int left = lo, right = lo;
        for (int r = mid; r < hi; r++)
        {
            while (left < mid && pref[left] < pref[r] - upper) left++;
            while (right < mid && pref[right] <= pref[r] - lower) right++;
            ans += right - left;
        }
        int i = lo, j = mid, k = lo;
        while (i < mid && j < hi) tmp[k++] = pref[i] <= pref[j] ? pref[i++] : pref[j++];
        while (i < mid) tmp[k++] = pref[i++];
        while (j < hi) tmp[k++] = pref[j++];
        Array.Copy(tmp, lo, pref, lo, hi - lo);
        return ans;
    }

    static int CountRangeSum(int[] nums, int lower, int upper)
    {
        long[] pref = new long[nums.Length + 1];
        for (int i = 0; i < nums.Length; i++) pref[i + 1] = pref[i] + nums[i];
        long[] tmp = new long[pref.Length];
        return Sort(pref, tmp, 0, pref.Length, lower, upper);
    }

    public static void Main()
    {
        string[] data = Console.In.ReadToEnd().Split((char[])null, StringSplitOptions.RemoveEmptyEntries);
        if (data.Length == 0) return;
        int idx = 0, t = int.Parse(data[idx++]);
        var outv = new List<string>();
        for (int tc = 0; tc < t; tc++)
        {
            int n = int.Parse(data[idx++]);
            int[] nums = new int[n];
            for (int i = 0; i < n; i++) nums[i] = int.Parse(data[idx++]);
            int lower = int.Parse(data[idx++]);
            int upper = int.Parse(data[idx++]);
            outv.Add(CountRangeSum(nums, lower, upper).ToString());
        }
        Console.Write(string.Join("\n\n", outv));
    }
}
