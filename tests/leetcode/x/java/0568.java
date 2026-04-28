public class Main {
    private static int solve(int[][] flights, int[][] days) {
        int n = flights.length;
        int weeks = days[0].length;
        int neg = -1_000_000_000;
        int[] dp = new int[n];
        java.util.Arrays.fill(dp, neg);
        dp[0] = 0;
        for (int week = 0; week < weeks; week++) {
            int[] ndp = new int[n];
            java.util.Arrays.fill(ndp, neg);
            for (int city = 0; city < n; city++) {
                if (dp[city] == neg) continue;
                for (int nxt = 0; nxt < n; nxt++) {
                    if (city == nxt || flights[city][nxt] == 1) {
                        ndp[nxt] = Math.max(ndp[nxt], dp[city] + days[nxt][week]);
                    }
                }
            }
            dp = ndp;
        }
        int best = dp[0];
        for (int v : dp) best = Math.max(best, v);
        return best;
    }

    public static void main(String[] args) throws Exception {
        FastScanner fs = new FastScanner();
        int t = fs.nextInt();
        if (t == Integer.MIN_VALUE) return;
        StringBuilder out = new StringBuilder();
        for (int tc = 0; tc < t; tc++) {
            int n = fs.nextInt();
            int w = fs.nextInt();
            int[][] flights = new int[n][n];
            int[][] days = new int[n][w];
            for (int i = 0; i < n; i++)
                for (int j = 0; j < n; j++)
                    flights[i][j] = fs.nextInt();
            for (int i = 0; i < n; i++)
                for (int j = 0; j < w; j++)
                    days[i][j] = fs.nextInt();
            if (tc > 0) out.append("\n\n");
            out.append(solve(flights, days));
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
