// Generated by Mochi 0.10.34 on 2025-07-22 09:58 +0700
using System;
class Program {
    static int[] result = twoSum(new int[]{2, 7, 11, 15}, 9);
    static int[] twoSum(int[] nums, int target) {
        var n = nums.Length;
        for (var i = 0; i < n; i++) {
            for (var j = (i + 1); j < n; j++) {
                if (((nums[i] + nums[j]) == target)) {
                    return new int[]{i, j};
                }
            }
        }
        return new object[]{-1, -1};
    }
    static void Main() {
        Console.WriteLine(result[0]);
        Console.WriteLine(result[1]);
    }
}
