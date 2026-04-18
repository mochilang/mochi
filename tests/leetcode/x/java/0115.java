import java.io.*;
import java.util.*;

public class Main {
    static int solve(String s, String t) {
        int[] dp = new int[t.length() + 1];
        dp[0] = 1;
        for (int i = 0; i < s.length(); i++) {
            for (int j = t.length(); j >= 1; j--) {
                if (s.charAt(i) == t.charAt(j - 1)) dp[j] += dp[j - 1];
            }
        }
        return dp[t.length()];
    }

    public static void main(String[] args) throws Exception {
        List<String> lines = Arrays.asList(new String(System.in.readAllBytes()).split("\\R", -1));
        if (lines.isEmpty() || lines.get(0).trim().isEmpty()) return;
        int tc = Integer.parseInt(lines.get(0).trim());
        StringBuilder out = new StringBuilder();
        for (int i = 0; i < tc; i++) {
            if (i > 0) out.append('\n');
            out.append(solve(lines.get(1 + 2 * i), lines.get(2 + 2 * i)));
        }
        System.out.print(out);
    }
}
