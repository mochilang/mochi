public class Main {
    private static final int MOD = 1_000_000_007;

    private static int solve(int n, int k) {
        int[] dp = new int[k + 1];
        dp[0] = 1;
        for (int num = 1; num <= n; num++) {
            int[] ndp = new int[k + 1];
            int window = 0;
            for (int inv = 0; inv <= k; inv++) {
                window += dp[inv];
                if (window >= MOD) window -= MOD;
                if (inv >= num) {
                    window -= dp[inv - num];
                    if (window < 0) window += MOD;
                }
                ndp[inv] = window;
            }
            dp = ndp;
        }
        return dp[k];
    }

    public static void main(String[] args) throws Exception {
        FastScanner fs = new FastScanner();
        int t = fs.nextInt();
        if (t == Integer.MIN_VALUE) return;
        StringBuilder out = new StringBuilder();
        for (int i = 0; i < t; i++) {
            int n = fs.nextInt();
            int k = fs.nextInt();
            if (i > 0) out.append("\n\n");
            out.append(solve(n, k));
        }
        System.out.print(out);
    }

    private static final class FastScanner {
        private final byte[] buffer = new byte[1 << 16];
        private int ptr = 0, len = 0;

        private int read() throws java.io.IOException {
            if (ptr >= len) {
                len = System.in.read(buffer);
                ptr = 0;
                if (len <= 0) return -1;
            }
            return buffer[ptr++];
        }

        int nextInt() throws java.io.IOException {
            int c;
            do {
                c = read();
                if (c == -1) return Integer.MIN_VALUE;
            } while (c <= ' ');
            int sign = 1;
            if (c == '-') {
                sign = -1;
                c = read();
            }
            int val = 0;
            while (c > ' ') {
                val = val * 10 + (c - '0');
                c = read();
            }
            return val * sign;
        }
    }
}
