using System;
using System.Collections.Generic;

class Program {
    static long BucketId(long x, long size) {
        return x >= 0 ? x / size : -((-x - 1) / size) - 1;
    }

    static bool Solve(long[] nums, int indexDiff, long valueDiff) {
        long size = valueDiff + 1;
        var buckets = new Dictionary<long, long>();
        for (int i = 0; i < nums.Length; i++) {
            long x = nums[i];
            long bid = BucketId(x, size);
            if (buckets.ContainsKey(bid)) return true;
            if (buckets.ContainsKey(bid - 1) && Math.Abs(x - buckets[bid - 1]) <= valueDiff) return true;
            if (buckets.ContainsKey(bid + 1) && Math.Abs(x - buckets[bid + 1]) <= valueDiff) return true;
            buckets[bid] = x;
            if (i >= indexDiff) buckets.Remove(BucketId(nums[i - indexDiff], size));
        }
        return false;
    }

    static void Main() {
        var toks = Console.In.ReadToEnd().Split((char[])null, StringSplitOptions.RemoveEmptyEntries);
        if (toks.Length == 0) return;
        int idx = 0, t = int.Parse(toks[idx++]);
        var outLines = new string[t];
        for (int tc = 0; tc < t; tc++) {
            int n = int.Parse(toks[idx++]);
            var nums = new long[n];
            for (int i = 0; i < n; i++) nums[i] = long.Parse(toks[idx++]);
            int indexDiff = int.Parse(toks[idx++]);
            long valueDiff = long.Parse(toks[idx++]);
            outLines[tc] = Solve(nums, indexDiff, valueDiff) ? "true" : "false";
        }
        Console.Write(string.Join("\n", outLines));
    }
}
