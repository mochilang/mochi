import java.io.BufferedInputStream;
import java.util.ArrayDeque;

public class Main {
    private static int[] solve(int[] nums, int k) {
        ArrayDeque<Integer> dq = new ArrayDeque<>();
        int[] ans = new int[nums.length - k + 1];
        int p = 0;
        for (int i = 0; i < nums.length; i++) {
            while (!dq.isEmpty() && dq.peekFirst() <= i - k) dq.pollFirst();
            while (!dq.isEmpty() && nums[dq.peekLast()] <= nums[i]) dq.pollLast();
            dq.offerLast(i);
            if (i >= k - 1) ans[p++] = nums[dq.peekFirst()];
        }
        return ans;
    }

    public static void main(String[] args) throws Exception {
        FastScanner fs = new FastScanner();
        int t = fs.nextInt();
        StringBuilder out = new StringBuilder();
        for (int tc = 0; tc < t; tc++) {
            int n = fs.nextInt();
            int[] nums = new int[n];
            for (int i = 0; i < n; i++) nums[i] = fs.nextInt();
            int k = fs.nextInt();
            int[] ans = solve(nums, k);
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
