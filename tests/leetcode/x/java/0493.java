import java.io.*;

class Main {
    static long sortCount(long[] a, int l, int r, long[] tmp) {
        if (r - l <= 1) return 0;
        int m = (l + r) / 2;
        long cnt = sortCount(a, l, m, tmp) + sortCount(a, m, r, tmp);
        int j = m;
        for (int i = l; i < m; i++) {
            while (j < r && a[i] > 2L * a[j]) j++;
            cnt += j - m;
        }
        int i = l, k = l;
        j = m;
        while (i < m && j < r) tmp[k++] = a[i] <= a[j] ? a[i++] : a[j++];
        while (i < m) tmp[k++] = a[i++];
        while (j < r) tmp[k++] = a[j++];
        for (int x = l; x < r; x++) a[x] = tmp[x];
        return cnt;
    }

    static long solve(long[] nums) {
        return sortCount(nums, 0, nums.length, new long[nums.length]);
    }

    public static void main(String[] args) throws Exception {
        String text = new String(System.in.readAllBytes()).trim();
        if (text.isEmpty()) return;
        String[] v = text.split("\\s+");
        int idx = 0, t = Integer.parseInt(v[idx++]);
        StringBuilder out = new StringBuilder();
        for (int tc = 0; tc < t; tc++) {
            int n = Integer.parseInt(v[idx++]);
            long[] nums = new long[n];
            for (int i = 0; i < n; i++) nums[i] = Long.parseLong(v[idx++]);
            if (tc > 0) out.append("\n\n");
            out.append(solve(nums));
        }
        System.out.print(out);
    }
}
