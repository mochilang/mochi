import java.io.BufferedInputStream;

public class Main {
    private static int[] solve(int[] values, double target, int k) {
        int right = 0;
        while (right < values.length && values[right] < target) right++;
        int left = right - 1;
        int[] ans = new int[k];
        for (int i = 0; i < k; i++) {
            if (left < 0) ans[i] = values[right++];
            else if (right >= values.length) ans[i] = values[left--];
            else if (Math.abs(values[left] - target) <= Math.abs(values[right] - target)) ans[i] = values[left--];
            else ans[i] = values[right++];
        }
        return ans;
    }

    public static void main(String[] args) throws Exception {
        FastScanner fs = new FastScanner();
        int t = fs.nextInt();
        StringBuilder out = new StringBuilder();
        for (int tc = 0; tc < t; tc++) {
            int n = fs.nextInt();
            int[] values = new int[n];
            for (int i = 0; i < n; i++) values[i] = fs.nextInt();
            double target = fs.nextDouble();
            int k = fs.nextInt();
            int[] ans = solve(values, target, k);
            if (tc > 0) out.append("\n\n");
            out.append(ans.length);
            for (int x : ans) out.append('\n').append(x);
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

        String next() throws Exception {
            int c;
            do c = read(); while (c <= ' ' && c != -1);
            StringBuilder sb = new StringBuilder();
            while (c > ' ') {
                sb.append((char)c);
                c = read();
            }
            return sb.toString();
        }

        int nextInt() throws Exception { return Integer.parseInt(next()); }
        double nextDouble() throws Exception { return Double.parseDouble(next()); }
    }
}
