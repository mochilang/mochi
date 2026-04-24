import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

public class Main {
    private static final class State {
        final int l;
        final int r;
        final int k;

        State(int l, int r, int k) {
            this.l = l;
            this.r = r;
            this.k = k;
        }

        @Override
        public boolean equals(Object obj) {
            if (!(obj instanceof State)) {
                return false;
            }
            State other = (State) obj;
            return l == other.l && r == other.r && k == other.k;
        }

        @Override
        public int hashCode() {
            return ((l * 131071) ^ (r * 8191)) ^ k;
        }
    }

    private static int solve(int[] boxes) {
        Map<State, Integer> memo = new HashMap<>();
        return dp(boxes, 0, boxes.length - 1, 0, memo);
    }

    private static int dp(int[] boxes, int l, int r, int k, Map<State, Integer> memo) {
        if (l > r) {
            return 0;
        }
        while (l < r && boxes[r] == boxes[r - 1]) {
            r--;
            k++;
        }
        State key = new State(l, r, k);
        Integer cached = memo.get(key);
        if (cached != null) {
            return cached;
        }
        int best = dp(boxes, l, r - 1, 0, memo) + (k + 1) * (k + 1);
        for (int i = l; i < r; i++) {
            if (boxes[i] == boxes[r]) {
                best = Math.max(best, dp(boxes, l, i, k + 1, memo) + dp(boxes, i + 1, r - 1, 0, memo));
            }
        }
        memo.put(key, best);
        return best;
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
            int[] boxes = new int[n];
            for (int i = 0; i < n; i++) {
                boxes[i] = fs.nextInt();
            }
            if (tc > 0) {
                out.append("\n\n");
            }
            out.append(solve(boxes));
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
