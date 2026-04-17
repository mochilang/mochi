import java.io.IOException;

public class Main {
    static class FastScanner {
        private final byte[] data;
        private int idx = 0;

        FastScanner() throws IOException {
            data = System.in.readAllBytes();
        }

        int nextInt() {
            while (idx < data.length && data[idx] <= ' ') idx++;
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
            while (idx < data.length && data[idx] <= ' ') idx++;
            return idx < data.length;
        }
    }

    static boolean isPalindrome(int x) {
        if (x < 0) return false;
        int original = x;
        long rev = 0;
        while (x > 0) {
            rev = rev * 10 + (x % 10);
            x /= 10;
        }
        return rev == original;
    }

    public static void main(String[] args) throws Exception {
        FastScanner fs = new FastScanner();
        if (!fs.hasNext()) return;
        int t = fs.nextInt();
        StringBuilder out = new StringBuilder();
        for (int i = 0; i < t; i++) {
            out.append(isPalindrome(fs.nextInt()) ? "true" : "false");
            if (i + 1 < t) out.append('\n');
        }
        System.out.print(out);
    }
}
