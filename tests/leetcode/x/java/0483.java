import java.io.*;

class Main {
    static long value(long base, int m, long limit) {
        long total = 1, cur = 1;
        for (int i = 0; i < m; i++) {
            if (cur > limit / base) return limit + 1;
            cur *= base;
            if (total > limit - cur) return limit + 1;
            total += cur;
        }
        return total;
    }

    static String solve(long n) {
        int maxM = 63 - Long.numberOfLeadingZeros(n);
        for (int m = maxM; m >= 2; m--) {
            long lo = 2, hi = (long)Math.pow(n, 1.0 / m) + 1;
            while (lo <= hi) {
                long mid = (lo + hi) / 2;
                long s = value(mid, m, n);
                if (s == n) return Long.toString(mid);
                if (s < n) lo = mid + 1;
                else hi = mid - 1;
            }
        }
        return Long.toString(n - 1);
    }

    public static void main(String[] args) throws Exception {
        String text = new String(System.in.readAllBytes()).trim();
        if (text.isEmpty()) return;
        String[] v = text.split("\\s+");
        int idx = 0, t = Integer.parseInt(v[idx++]);
        StringBuilder out = new StringBuilder();
        for (int tc = 0; tc < t; tc++) {
            long n = Long.parseLong(v[idx++]);
            if (tc > 0) out.append("\n\n");
            out.append(solve(n));
        }
        System.out.print(out);
    }
}
