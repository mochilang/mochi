import java.io.*;

public class Main {
    static boolean solve(String s1, String s2, String s3) {
        int m = s1.length(), n = s2.length();
        if (m + n != s3.length()) return false;
        boolean[][] dp = new boolean[m + 1][n + 1];
        dp[0][0] = true;
        for (int i = 0; i <= m; i++) for (int j = 0; j <= n; j++) {
            if (i > 0 && dp[i - 1][j] && s1.charAt(i - 1) == s3.charAt(i + j - 1)) dp[i][j] = true;
            if (j > 0 && dp[i][j - 1] && s2.charAt(j - 1) == s3.charAt(i + j - 1)) dp[i][j] = true;
        }
        return dp[m][n];
    }

    public static void main(String[] args) throws Exception {
        String[] lines = new String(System.in.readAllBytes()).split("\\R", -1);
        if (lines.length == 0 || lines[0].trim().isEmpty()) return;
        int t = Integer.parseInt(lines[0].trim());
        StringBuilder out = new StringBuilder();
        for (int i = 0; i < t; i++) {
            if (i > 0) out.append('\n');
            out.append(solve(lines[1 + 3 * i], lines[2 + 3 * i], lines[3 + 3 * i]) ? "true" : "false");
        }
        System.out.print(out);
    }
}
