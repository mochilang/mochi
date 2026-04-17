import java.io.IOException;

public class Main {
    static class FastScanner {
        private final byte[] data;
        private int idx = 0;

        FastScanner() throws IOException {
            data = System.in.readAllBytes();
        }

        int nextInt() {
            while (idx < data.length && data[idx] <= ' ') {
                idx++;
            }
            int sign = 1;
            if (data[idx] == '-') {
                sign = -1;
                idx++;
            }
            int value = 0;
            while (idx < data.length && data[idx] > ' ') {
                value = value * 10 + (data[idx] - '0');
                idx++;
            }
            return sign * value;
        }

        boolean hasNext() {
            while (idx < data.length && data[idx] <= ' ') {
                idx++;
            }
            return idx < data.length;
        }
    }

    static int[] twoSum(int[] nums, int target) {
        for (int i = 0; i < nums.length; i++) {
            for (int j = i + 1; j < nums.length; j++) {
                if (nums[i] + nums[j] == target) {
                    return new int[] {i, j};
                }
            }
        }
        return new int[] {0, 0};
    }

    public static void main(String[] args) throws Exception {
        FastScanner fs = new FastScanner();
        if (!fs.hasNext()) {
            return;
        }
        int t = fs.nextInt();
        StringBuilder out = new StringBuilder();
        for (int tc = 0; tc < t; tc++) {
            int n = fs.nextInt();
            int target = fs.nextInt();
            int[] nums = new int[n];
            for (int i = 0; i < n; i++) {
                nums[i] = fs.nextInt();
            }
            int[] ans = twoSum(nums, target);
            out.append(ans[0]).append(' ').append(ans[1]);
            if (tc + 1 < t) {
                out.append('\n');
            }
        }
        System.out.print(out);
    }
}
