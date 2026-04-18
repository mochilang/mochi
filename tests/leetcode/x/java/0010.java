import java.io.*;
import java.util.*;

public class Main {
    static boolean matchAt(String s, String p, int i, int j) {
        if (j == p.length()) return i == s.length();
        boolean first = i < s.length() && (p.charAt(j) == '.' || s.charAt(i) == p.charAt(j));
        if (j + 1 < p.length() && p.charAt(j + 1) == '*') {
            return matchAt(s, p, i, j + 2) || (first && matchAt(s, p, i + 1, j));
        }
        return first && matchAt(s, p, i + 1, j + 1);
    }

    public static void main(String[] args) throws Exception {
        BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        String first = br.readLine();
        if (first == null || first.trim().isEmpty()) return;
        int t = Integer.parseInt(first.trim());
        StringBuilder out = new StringBuilder();
        for (int tc = 0; tc < t; tc++) {
            String s = br.readLine();
            String p = br.readLine();
            out.append(matchAt(s, p, 0, 0) ? "true" : "false");
            if (tc + 1 < t) out.append('\n');
        }
        System.out.print(out);
    }
}
