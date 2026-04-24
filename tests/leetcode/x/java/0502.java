import java.io.IOException;
import java.util.Arrays;
import java.util.Collections;
import java.util.PriorityQueue;

public class Main {
    private static int solve(int k, int w, int[] profits, int[] capital) {
        int n = profits.length;
        int[][] projects = new int[n][2];
        for (int i = 0; i < n; i++) {
            projects[i][0] = capital[i];
            projects[i][1] = profits[i];
        }
        Arrays.sort(projects, (a, b) -> Integer.compare(a[0], b[0]));
        PriorityQueue<Integer> pq = new PriorityQueue<>(Collections.reverseOrder());
        int cur = w;
        int idx = 0;
        for (int step = 0; step < k; step++) {
            while (idx < n && projects[idx][0] <= cur) {
                pq.offer(projects[idx][1]);
                idx++;
            }
            if (pq.isEmpty()) {
                break;
            }
            cur += pq.poll();
        }
        return cur;
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
            int k = fs.nextInt();
            int w = fs.nextInt();
            int[] profits = new int[n];
            int[] capital = new int[n];
            for (int i = 0; i < n; i++) {
                profits[i] = fs.nextInt();
            }
            for (int i = 0; i < n; i++) {
                capital[i] = fs.nextInt();
            }
            if (tc > 0) {
                out.append("\n\n");
            }
            out.append(solve(k, w, profits, capital));
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
