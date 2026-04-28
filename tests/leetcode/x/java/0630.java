import java.util.Arrays;
import java.util.PriorityQueue;

public class Main {
    private static int solve(int[][] courses) {
        Arrays.sort(courses, (a, b) -> Integer.compare(a[1], b[1]));
        int total = 0;
        PriorityQueue<Integer> pq = new PriorityQueue<>((a, b) -> Integer.compare(b, a));
        for (int[] c : courses) {
            total += c[0];
            pq.offer(c[0]);
            if (total > c[1]) {
                total -= pq.poll();
            }
        }
        return pq.size();
    }

    public static void main(String[] args) throws Exception {
        FastScanner fs = new FastScanner();
        int t = fs.nextInt();
        if (t == Integer.MIN_VALUE) return;
        StringBuilder out = new StringBuilder();
        for (int i = 0; i < t; i++) {
            int n = fs.nextInt();
            int[][] courses = new int[n][2];
            for (int j = 0; j < n; j++) {
                courses[j][0] = fs.nextInt();
                courses[j][1] = fs.nextInt();
            }
            if (i > 0) out.append("\n\n");
            out.append(solve(courses));
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
