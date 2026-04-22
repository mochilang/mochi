import java.io.BufferedInputStream;

public class Main {
    private static int solve(int[][] costs) {
        if (costs.length == 0) return 0;
        int[] prev = costs[0].clone();
        for (int r = 1; r < costs.length; r++) {
            int min1 = Integer.MAX_VALUE;
            int min2 = Integer.MAX_VALUE;
            int idx1 = -1;
            for (int i = 0; i < prev.length; i++) {
                if (prev[i] < min1) {
                    min2 = min1;
                    min1 = prev[i];
                    idx1 = i;
                } else if (prev[i] < min2) {
                    min2 = prev[i];
                }
            }
            int[] cur = new int[prev.length];
            for (int i = 0; i < prev.length; i++) {
                cur[i] = costs[r][i] + (i == idx1 ? min2 : min1);
            }
            prev = cur;
        }
        int ans = prev[0];
        for (int v : prev) ans = Math.min(ans, v);
        return ans;
    }

    public static void main(String[] args) throws Exception {
        FastScanner fs = new FastScanner();
        int t = fs.nextInt();
        StringBuilder out = new StringBuilder();
        for (int tc = 0; tc < t; tc++) {
            int n = fs.nextInt();
            int k = fs.nextInt();
            int[][] costs = new int[n][k];
            for (int i = 0; i < n; i++) for (int j = 0; j < k; j++) costs[i][j] = fs.nextInt();
            if (tc > 0) out.append('\n');
            out.append(solve(costs));
        }
        System.out.print(out);
    }

    static class FastScanner {
        private final BufferedInputStream in = new BufferedInputStream(System.in);
        private final byte[] buffer = new byte[1 << 16];
        private int ptr = 0, len = 0;

        private int read() throws Exception {
            if (ptr >= len) {
                len = in.read(buffer);
                ptr = 0;
                if (len <= 0) return -1;
            }
            return buffer[ptr++];
        }

        int nextInt() throws Exception {
            int c;
            do c = read(); while (c <= ' ' && c != -1);
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
