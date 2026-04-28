public class Main {
    private static int solve(int n) {
        int[] f = new int[32];
        f[0] = 1;
        f[1] = 2;
        for (int i = 2; i < 32; i++) {
            f[i] = f[i - 1] + f[i - 2];
        }

        int ans = 0;
        int prev = 0;
        for (int i = 30; i >= 0; i--) {
            if ((n & (1 << i)) != 0) {
                ans += f[i];
                if (prev == 1) return ans;
                prev = 1;
            } else {
                prev = 0;
            }
        }
        return ans + 1;
    }

    public static void main(String[] args) throws Exception {
        FastScanner fs = new FastScanner();
        int t = fs.nextInt();
        if (t == Integer.MIN_VALUE) return;
        StringBuilder out = new StringBuilder();
        for (int i = 0; i < t; i++) {
            int n = fs.nextInt();
            if (i > 0) out.append("\n\n");
            out.append(solve(n));
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
