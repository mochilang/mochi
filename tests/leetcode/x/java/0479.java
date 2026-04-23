import java.io.*;

class Main {
    static long makePal(long left) {
        String s = Long.toString(left);
        StringBuilder b = new StringBuilder(s);
        return Long.parseLong(s + b.reverse());
    }

    static int solve(int n) {
        if (n == 1) return 9;
        long upper = 1;
        for (int i = 0; i < n; i++) upper *= 10;
        long lower = upper / 10;
        upper--;
        for (long left = upper; left >= lower; left--) {
            long pal = makePal(left);
            for (long x = upper; x * x >= pal; x--) {
                if (pal % x == 0) {
                    long y = pal / x;
                    if (y >= lower && y <= upper) return (int)(pal % 1337);
                }
            }
        }
        return -1;
    }

    public static void main(String[] args) throws Exception {
        String text = new String(System.in.readAllBytes()).trim();
        if (text.isEmpty()) return;
        String[] v = text.split("\\s+");
        int idx = 0, t = Integer.parseInt(v[idx++]);
        StringBuilder out = new StringBuilder();
        for (int tc = 0; tc < t; tc++) {
            int n = Integer.parseInt(v[idx++]);
            if (tc > 0) out.append("\n\n");
            out.append(solve(n));
        }
        System.out.print(out);
    }
}
