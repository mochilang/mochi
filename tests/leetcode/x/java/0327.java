import java.io.*;
import java.util.*;

class Main {
    static int countRangeSum(int[] nums, int lower, int upper) {
        long[] pref = new long[nums.length + 1];
        for (int i = 0; i < nums.length; i++) pref[i + 1] = pref[i] + nums[i];
        long[] tmp = new long[pref.length];
        return sort(pref, tmp, 0, pref.length, lower, upper);
    }

    static int sort(long[] pref, long[] tmp, int lo, int hi, int lower, int upper) {
        if (hi - lo <= 1) return 0;
        int mid = (lo + hi) / 2;
        int ans = sort(pref, tmp, lo, mid, lower, upper) + sort(pref, tmp, mid, hi, lower, upper);
        int left = lo, right = lo;
        for (int r = mid; r < hi; r++) {
            while (left < mid && pref[left] < pref[r] - upper) left++;
            while (right < mid && pref[right] <= pref[r] - lower) right++;
            ans += right - left;
        }
        int i = lo, j = mid, k = lo;
        while (i < mid && j < hi) tmp[k++] = pref[i] <= pref[j] ? pref[i++] : pref[j++];
        while (i < mid) tmp[k++] = pref[i++];
        while (j < hi) tmp[k++] = pref[j++];
        System.arraycopy(tmp, lo, pref, lo, hi - lo);
        return ans;
    }

    public static void main(String[] args) throws Exception {
        FastScanner fs = new FastScanner(System.in);
        Integer tt = fs.nextIntOrNull();
        if (tt == null) return;
        StringBuilder out = new StringBuilder();
        for (int tc = 0; tc < tt; tc++) {
            int n = fs.nextInt();
            int[] nums = new int[n];
            for (int i = 0; i < n; i++) nums[i] = fs.nextInt();
            int lower = fs.nextInt(), upper = fs.nextInt();
            if (tc > 0) out.append("\n\n");
            out.append(countRangeSum(nums, lower, upper));
        }
        System.out.print(out);
    }

    static class FastScanner {
        private final byte[] data;
        private int idx = 0;
        FastScanner(InputStream is) throws IOException { data = is.readAllBytes(); }
        Integer nextIntOrNull() { skip(); if (idx >= data.length) return null; return nextInt(); }
        int nextInt() { skip(); int sign = 1; if (data[idx] == '-') { sign = -1; idx++; } int v = 0; while (idx < data.length && data[idx] > ' ') v = v * 10 + data[idx++] - '0'; return v * sign; }
        void skip() { while (idx < data.length && data[idx] <= ' ') idx++; }
    }
}
