using System;
using System.Collections.Generic;

public class Program
{
    static void Sort(int[] nums, int[] idx, int[] tmp, int[] counts, int lo, int hi)
    {
        if (hi - lo <= 1) return;
        int mid = (lo + hi) / 2;
        Sort(nums, idx, tmp, counts, lo, mid);
        Sort(nums, idx, tmp, counts, mid, hi);
        int i = lo, j = mid, k = lo, moved = 0;
        while (i < mid && j < hi)
        {
            if (nums[idx[j]] < nums[idx[i]])
            {
                tmp[k++] = idx[j++];
                moved++;
            }
            else
            {
                counts[idx[i]] += moved;
                tmp[k++] = idx[i++];
            }
        }
        while (i < mid)
        {
            counts[idx[i]] += moved;
            tmp[k++] = idx[i++];
        }
        while (j < hi) tmp[k++] = idx[j++];
        for (int p = lo; p < hi; p++) idx[p] = tmp[p];
    }

    static List<int> CountSmaller(int[] nums)
    {
        int n = nums.Length;
        int[] counts = new int[n];
        int[] idx = new int[n];
        int[] tmp = new int[n];
        for (int i = 0; i < n; i++) idx[i] = i;
        Sort(nums, idx, tmp, counts, 0, n);
        return new List<int>(counts);
    }

    static string FmtList(List<int> a) => "[" + string.Join(",", a) + "]";

    public static void Main()
    {
        string[] data = Console.In.ReadToEnd().Split((char[])null, StringSplitOptions.RemoveEmptyEntries);
        if (data.Length == 0) return;
        int pos = 0;
        int t = int.Parse(data[pos++]);
        var blocks = new List<string>();
        for (int tc = 0; tc < t; tc++)
        {
            int n = int.Parse(data[pos++]);
            int[] nums = new int[n];
            for (int i = 0; i < n; i++) nums[i] = int.Parse(data[pos++]);
            blocks.Add(FmtList(CountSmaller(nums)));
        }
        Console.Write(string.Join("\n\n", blocks));
    }
}
