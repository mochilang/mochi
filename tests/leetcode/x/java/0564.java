import java.io.IOException;
import java.util.HashSet;
import java.util.Set;

public class Main {
    private static long pow10(int e) {
        long v = 1;
        while (e-- > 0) {
            v *= 10L;
        }
        return v;
    }

    private static long makePal(String prefix, boolean odd) {
        StringBuilder sb = new StringBuilder(prefix);
        for (int i = prefix.length() - 1 - (odd ? 1 : 0); i >= 0; i--) {
            sb.append(prefix.charAt(i));
        }
        return Long.parseLong(sb.toString());
    }

    private static String solve(String n) {
        int m = n.length();
        long x = Long.parseLong(n);
        Set<Long> cands = new HashSet<>();
        cands.add(pow10(m - 1) - 1);
        cands.add(pow10(m) + 1);
        long prefix = Long.parseLong(n.substring(0, (m + 1) / 2));
        for (long d = -1; d <= 1; d++) {
            cands.add(makePal(Long.toString(prefix + d), (m & 1) == 1));
        }
        cands.remove(x);
        long best = -1;
        for (long cand : cands) {
            if (cand < 0) {
                continue;
            }
            if (best == -1 || Math.abs(cand - x) < Math.abs(best - x) ||
                (Math.abs(cand - x) == Math.abs(best - x) && cand < best)) {
                best = cand;
            }
        }
        return Long.toString(best);
    }

    public static void main(String[] args) throws Exception {
        FastScanner fs = new FastScanner();
        int t = fs.nextInt();
        if (t == Integer.MIN_VALUE) {
            return;
        }
        StringBuilder out = new StringBuilder();
        for (int i = 0; i < t; i++) {
            String n = fs.next();
            if (i > 0) {
                out.append("\n\n");
            }
            out.append(solve(n));
        }
        System.out.print(out);
    }

    private static final class FastScanner {
        private final byte[] buffer = new byte[1 << 16];
        private int ptr = 0;
        private int len = 0;

        private int read() throws IOException {
            if (ptr >= len) {
                len = System.in.read(buffer);
                ptr = 0;
                if (len <= 0) {
                    return -1;
                }
            }
            return buffer[ptr++];
        }

        int nextInt() throws IOException {
            String s = next();
            if (s == null) return Integer.MIN_VALUE;
            return Integer.parseInt(s);
        }

        String next() throws IOException {
            int c;
            do {
                c = read();
                if (c == -1) return null;
            } while (c <= ' ');
            StringBuilder sb = new StringBuilder();
            while (c > ' ') {
                sb.append((char) c);
                c = read();
            }
            return sb.toString();
        }
    }
}
