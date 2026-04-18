import java.io.*;
import java.util.*;

public class Main {
    static String s1, s2;
    static Map<String, Boolean> memo;

    static boolean dfs(int i1, int i2, int len) {
        String key = i1 + "," + i2 + "," + len;
        if (memo.containsKey(key)) return memo.get(key);
        String a = s1.substring(i1, i1 + len), b = s2.substring(i2, i2 + len);
        if (a.equals(b)) return memo.put(key, true) == null || true;
        int[] cnt = new int[26];
        for (int i = 0; i < len; i++) {
            cnt[a.charAt(i) - 'a']++;
            cnt[b.charAt(i) - 'a']--;
        }
        for (int v : cnt) if (v != 0) return memo.put(key, false) == null && false;
        for (int k = 1; k < len; k++) {
            if ((dfs(i1, i2, k) && dfs(i1 + k, i2 + k, len - k)) ||
                (dfs(i1, i2 + len - k, k) && dfs(i1 + k, i2, len - k))) {
                memo.put(key, true);
                return true;
            }
        }
        memo.put(key, false);
        return false;
    }

    static boolean isScramble(String a, String b) {
        s1 = a; s2 = b; memo = new HashMap<>();
        return dfs(0, 0, a.length());
    }

    public static void main(String[] args) throws Exception {
        String[] lines = new String(System.in.readAllBytes()).split("\\R", -1);
        if (lines.length == 0 || lines[0].trim().isEmpty()) return;
        int t = Integer.parseInt(lines[0].trim());
        StringBuilder out = new StringBuilder();
        for (int i = 0; i < t; i++) {
            if (i > 0) out.append('\n');
            out.append(isScramble(lines[1 + 2 * i], lines[2 + 2 * i]) ? "true" : "false");
        }
        System.out.print(out);
    }
}
