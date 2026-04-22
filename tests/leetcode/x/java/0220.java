import java.util.*;

public class Main {
    static long bucketId(long x, long size) {
        return x >= 0 ? x / size : -((-x - 1) / size) - 1;
    }

    static boolean solve(long[] nums, int indexDiff, long valueDiff) {
        long size = valueDiff + 1;
        Map<Long, Long> buckets = new HashMap<>();
        for (int i = 0; i < nums.length; i++) {
            long x = nums[i];
            long bid = bucketId(x, size);
            if (buckets.containsKey(bid)) return true;
            if (buckets.containsKey(bid - 1) && Math.abs(x - buckets.get(bid - 1)) <= valueDiff) return true;
            if (buckets.containsKey(bid + 1) && Math.abs(x - buckets.get(bid + 1)) <= valueDiff) return true;
            buckets.put(bid, x);
            if (i >= indexDiff) buckets.remove(bucketId(nums[i - indexDiff], size));
        }
        return false;
    }

    public static void main(String[] args) throws Exception {
        String input = new String(System.in.readAllBytes()).trim();
        if (input.isEmpty()) return;
        String[] toks = input.split("\\s+");
        int idx = 0, t = Integer.parseInt(toks[idx++]);
        StringBuilder out = new StringBuilder();
        for (int tc = 0; tc < t; tc++) {
            int n = Integer.parseInt(toks[idx++]);
            long[] nums = new long[n];
            for (int i = 0; i < n; i++) nums[i] = Long.parseLong(toks[idx++]);
            int indexDiff = Integer.parseInt(toks[idx++]);
            long valueDiff = Long.parseLong(toks[idx++]);
            if (tc > 0) out.append('\n');
            out.append(solve(nums, indexDiff, valueDiff) ? "true" : "false");
        }
        System.out.print(out);
    }
}
