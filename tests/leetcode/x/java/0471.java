import java.io.*;

class Main {
    static String better(String a, String b) {
        if (a.length() != b.length()) return a.length() < b.length() ? a : b;
        return a.compareTo(b) <= 0 ? a : b;
    }

    static int repeatLen(String s) {
        String doubled = s + s;
        int pos = doubled.indexOf(s, 1);
        if (pos >= 0 && pos < s.length() && s.length() % pos == 0) return pos;
        return s.length();
    }

    static String encode(String s) {
        int n = s.length();
        String[][] dp = new String[n][n];
        for (int len = 1; len <= n; len++) {
            for (int i = 0; i + len <= n; i++) {
                int j = i + len - 1;
                String sub = s.substring(i, j + 1);
                String best = sub;
                for (int k = i; k < j; k++) best = better(best, dp[i][k] + dp[k + 1][j]);
                int part = repeatLen(sub);
                if (part < len) best = better(best, (len / part) + "[" + dp[i][i + part - 1] + "]");
                dp[i][j] = best;
            }
        }
        return dp[0][n - 1];
    }

    public static void main(String[] args) throws Exception {
        String text = new String(System.in.readAllBytes()).trim();
        if (text.isEmpty()) return;
        String[] v = text.split("\\s+");
        int t = Integer.parseInt(v[0]);
        StringBuilder out = new StringBuilder();
        for (int tc = 0; tc < t; tc++) {
            if (tc > 0) out.append("\n\n");
            out.append(encode(v[tc + 1]));
        }
        System.out.print(out);
    }
}
