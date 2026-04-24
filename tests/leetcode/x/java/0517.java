public class Main {
    private static int solve(int[] machines) {
        int total = 0;
        for (int x : machines) {
            total += x;
        }
        int n = machines.length;
        if (total % n != 0) {
            return -1;
        }
        int target = total / n;
        int flow = 0;
        int ans = 0;
        for (int x : machines) {
            int diff = x - target;
            flow += diff;
            ans = Math.max(ans, Math.abs(flow));
            ans = Math.max(ans, diff);
        }
        return ans;
    }

    public static void main(String[] args) throws Exception {
        FastScanner fs = new FastScanner();
        int t = fs.nextInt();
        if (t == Integer.MIN_VALUE) {
            return;
        }
        StringBuilder out = new StringBuilder();
        for (int tc = 0; tc < t; tc++) {
            int n = fs.nextInt();
            int[] machines = new int[n];
            for (int i = 0; i < n; i++) {
                machines[i] = fs.nextInt();
            }
            if (tc > 0) {
                out.append("\n\n");
            }
            out.append(solve(machines));
        }
        System.out.print(out);
    }

    private static final class FastScanner {
        private final byte[] buffer = new byte[1 << 16];
        private int ptr = 0;
        private int len = 0;

        private int read() throws java.io.IOException {
            if (ptr >= len) {
                len = System.in.read(buffer);
                ptr = 0;
                if (len <= 0) {
                    return -1;
                }
            }
            return buffer[ptr++];
        }

        int nextInt() throws java.io.IOException {
            int c;
            do {
                c = read();
                if (c == -1) {
                    return Integer.MIN_VALUE;
                }
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
