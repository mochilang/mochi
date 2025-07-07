using System;

class Program
{
    static int[] TwoSum(int[] nums, int target)
    {
        int n = nums.Length;
        for (int i = 0; i < n; i++)
        {
            for (int j = i + 1; j < n; j++)
            {
                if (nums[i] + nums[j] == target)
                {
                    return new[] { i, j };
                }
            }
        }
        return new[] { -1, -1 };
    }

    static void Main()
    {
        var result = TwoSum(new[] { 2, 7, 11, 15 }, 9);
        Console.WriteLine(result[0]);
        Console.WriteLine(result[1]);
    }
}
