using System;
using System.Collections.Generic;

public class Program
{
    static int MinPatches(int[] nums, int n)
    {
        long miss = 1; int i = 0, patches = 0;
        while (miss <= n)
        {
            if (i < nums.Length && nums[i] <= miss) miss += nums[i++];
            else { miss += miss; patches++; }
        }
        return patches;
    }
    public static void Main()
    {
        string[] data = Console.In.ReadToEnd().Split((char[])null, StringSplitOptions.RemoveEmptyEntries); if (data.Length == 0) return;
        int idx = 0, t = int.Parse(data[idx++]); var outv = new List<string>();
        for (int tc = 0; tc < t; tc++) { int size = int.Parse(data[idx++]); int[] nums = new int[size]; for (int i = 0; i < size; i++) nums[i] = int.Parse(data[idx++]); int n = int.Parse(data[idx++]); outv.Add(MinPatches(nums, n).ToString()); }
        Console.Write(string.Join("\n\n", outv));
    }
}
