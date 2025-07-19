// Mochi 0.10.31 - generated 2025-07-19 13:00:19 UTC
using System;

class Program {
    static int twoSum(int nums, int target) {
    var n = nums.Length;
    for (var i = 0; i < n; i++) {
    for (var j = (i + 1); j < n; j++) {
    if (((nums[i] + nums[j]) == target ? 1 : 0) != 0) {
    return new[]{i, j};
};
};
};
    return new[]{-1, -1};
}
    static void Main() {
        var result = twoSum(new[]{2, 7, 11, 15}, 9);
        Console.WriteLine(result[0]);
        Console.WriteLine(result[1]);
    }
}
