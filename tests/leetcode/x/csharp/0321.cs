using System;
using System.Collections.Generic;

public class Program
{
    static int[] Pick(int[] nums, int k)
    {
        int drop = nums.Length - k;
        var stack = new List<int>();
        foreach (int x in nums)
        {
            while (drop > 0 && stack.Count > 0 && stack[stack.Count - 1] < x)
            {
                stack.RemoveAt(stack.Count - 1);
                drop--;
            }
            stack.Add(x);
        }
        return stack.GetRange(0, k).ToArray();
    }

    static bool Greater(int[] a, int i, int[] b, int j)
    {
        while (i < a.Length && j < b.Length && a[i] == b[j])
        {
            i++;
            j++;
        }
        return j == b.Length || (i < a.Length && a[i] > b[j]);
    }

    static int[] Merge(int[] a, int[] b)
    {
        int[] outv = new int[a.Length + b.Length];
        int i = 0, j = 0;
        for (int p = 0; p < outv.Length; p++)
        {
            if (Greater(a, i, b, j)) outv[p] = a[i++];
            else outv[p] = b[j++];
        }
        return outv;
    }

    static int[] MaxNumber(int[] nums1, int[] nums2, int k)
    {
        int[] best = new int[0];
        int start = Math.Max(0, k - nums2.Length);
        int end = Math.Min(k, nums1.Length);
        for (int take = start; take <= end; take++)
        {
            int[] cand = Merge(Pick(nums1, take), Pick(nums2, k - take));
            if (Greater(cand, 0, best, 0)) best = cand;
        }
        return best;
    }

    static string FmtList(int[] a) => "[" + string.Join(",", a) + "]";

    public static void Main()
    {
        string[] data = Console.In.ReadToEnd().Split((char[])null, StringSplitOptions.RemoveEmptyEntries);
        if (data.Length == 0) return;
        int idx = 0;
        int t = int.Parse(data[idx++]);
        var blocks = new List<string>();
        for (int tc = 0; tc < t; tc++)
        {
            int n1 = int.Parse(data[idx++]);
            int[] nums1 = new int[n1];
            for (int i = 0; i < n1; i++) nums1[i] = int.Parse(data[idx++]);
            int n2 = int.Parse(data[idx++]);
            int[] nums2 = new int[n2];
            for (int i = 0; i < n2; i++) nums2[i] = int.Parse(data[idx++]);
            int k = int.Parse(data[idx++]);
            blocks.Add(FmtList(MaxNumber(nums1, nums2, k)));
        }
        Console.Write(string.Join("\n\n", blocks));
    }
}
