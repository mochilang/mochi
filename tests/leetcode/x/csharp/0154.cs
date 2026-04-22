using System;
using System.Collections.Generic;

class Program {
    static int FindMin(List<int> nums) {
        int left = 0;
        int right = nums.Count - 1;
        while (left < right) {
            int mid = (left + right) / 2;
            if (nums[mid] < nums[right]) right = mid;
            else if (nums[mid] > nums[right]) left = mid + 1;
            else right--;
        }
        return nums[left];
    }

    static void Main() {
        var lines = new List<string>();
        string line;
        while ((line = Console.ReadLine()) != null) lines.Add(line);
        if (lines.Count == 0) return;
        int tc = int.Parse(lines[0]);
        int idx = 1;
        var outp = new List<string>();
        for (int t = 0; t < tc; t++) {
            int n = int.Parse(lines[idx++]);
            var nums = new List<int>();
            for (int i = 0; i < n; i++) nums.Add(int.Parse(lines[idx++]));
            outp.Add(FindMin(nums).ToString());
        }
        Console.Write(string.Join("\n\n", outp));
    }
}
