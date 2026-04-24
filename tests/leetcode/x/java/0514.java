import java.io.BufferedInputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

public class Main {
    private static int solve(String ring, String key) {
        int n = ring.length();
        Map<Character, ArrayList<Integer>> positions = new HashMap<>();
        for (int i = 0; i < n; i++) {
            positions.computeIfAbsent(ring.charAt(i), k -> new ArrayList<>()).add(i);
        }
        Map<Integer, Integer> dp = new HashMap<>();
        dp.put(0, 0);
        for (int idx = 0; idx < key.length(); idx++) {
            char ch = key.charAt(idx);
            Map<Integer, Integer> next = new HashMap<>();
            for (int j : positions.get(ch)) {
                int best = Integer.MAX_VALUE / 4;
                for (Map.Entry<Integer, Integer> entry : dp.entrySet()) {
                    int i = entry.getKey();
                    int cost = entry.getValue();
                    int diff = Math.abs(i - j);
                    int step = Math.min(diff, n - diff);
                    best = Math.min(best, cost + step);
                }
                next.put(j, best);
            }
            dp = next;
        }
        int ans = Integer.MAX_VALUE / 4;
        for (int cost : dp.values()) {
            ans = Math.min(ans, cost);
        }
        return ans + key.length();
    }

    public static void main(String[] args) throws Exception {
        FastScanner fs = new FastScanner();
        int t = fs.nextInt();
        if (t == Integer.MIN_VALUE) {
            return;
        }
        StringBuilder out = new StringBuilder();
        for (int tc = 0; tc < t; tc++) {
            String ring = fs.next();
            String key = fs.next();
            if (tc > 0) {
                out.append("\n\n");
            }
            out.append(solve(ring, key));
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
            if (s == null) {
                return Integer.MIN_VALUE;
            }
            return Integer.parseInt(s);
        }

        String next() throws IOException {
            int c;
            do {
                c = read();
                if (c == -1) {
                    return null;
                }
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
