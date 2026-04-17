using System;

public static class Program
{
    static int[] TwoSum(int[] nums, int target)
    {
        for (int i = 0; i < nums.Length; i++)
        {
            for (int j = i + 1; j < nums.Length; j++)
            {
                if (nums[i] + nums[j] == target)
                {
                    return new int[] { i, j };
                }
            }
        }
        return new int[] { 0, 0 };
    }

    public static void Main()
    {
        string input = Console.In.ReadToEnd();
        string[] parts = input.Split((char[])null, StringSplitOptions.RemoveEmptyEntries);
        if (parts.Length == 0)
        {
            return;
        }
        int idx = 0;
        int t = int.Parse(parts[idx++]);
        for (int tc = 0; tc < t; tc++)
        {
            int n = int.Parse(parts[idx++]);
            int target = int.Parse(parts[idx++]);
            int[] nums = new int[n];
            for (int i = 0; i < n; i++)
            {
                nums[i] = int.Parse(parts[idx++]);
            }
            int[] ans = TwoSum(nums, target);
            Console.WriteLine(ans[0] + " " + ans[1]);
        }
    }
}
