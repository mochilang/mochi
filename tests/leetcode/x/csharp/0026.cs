using System;
using System.Collections.Generic;
using System.Linq;

public class Solution {
    public int RemoveDuplicates(int[] nums) {
        if (nums.Length == 0) return 0;
        int k = 1;
        for (int i = 1; i < nums.Length; i++) {
            if (nums[i] != nums[k - 1]) {
                nums[k] = nums[i];
                k++;
            }
        }
        return k;
    }

    public static void Main(string[] args) {
        string input = Console.In.ReadToEnd();
        string[] tokens = input.Split(new char[] { ' ', '\n', '\r', '\t' }, StringSplitOptions.RemoveEmptyEntries);
        if (tokens.Length == 0) return;
        int idx = 0;
        int t = int.Parse(tokens[idx++]);
        Solution sol = new Solution();
        while (t-- > 0) {
            int n = int.Parse(tokens[idx++]);
            int[] nums = new int[n];
            for (int i = 0; i < n; i++) {
                nums[i] = int.Parse(tokens[idx++]);
            }
            int k = sol.RemoveDuplicates(nums);
            Console.WriteLine(string.Join(" ", nums.Take(k)));
        }
    }
}
