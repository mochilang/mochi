import java.io.*;
import java.util.*;

public class Main {
    static boolean isMatch(String s, String p) {
        int i = 0, j = 0, star = -1, match = 0;
        while (i < s.length()) {
            if (j < p.length() && (p.charAt(j) == '?' || p.charAt(j) == s.charAt(i))) { i++; j++; }
            else if (j < p.length() && p.charAt(j) == '*') { star = j; match = i; j++; }
            else if (star != -1) { j = star + 1; match++; i = match; }
            else return false;
        }
        while (j < p.length() && p.charAt(j) == '*') j++;
        return j == p.length();
    }
    public static void main(String[] args) throws Exception {
        BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        List<String> lines = new ArrayList<>();
        String line;
        while ((line = br.readLine()) != null) lines.add(line);
        if (lines.isEmpty() || lines.get(0).trim().isEmpty()) return;
        int idx = 0;
        int t = Integer.parseInt(lines.get(idx++).trim());
        StringBuilder out = new StringBuilder();
        for (int tc = 0; tc < t; tc++) {
            int n = Integer.parseInt(lines.get(idx++).trim());
            String s = n > 0 ? lines.get(idx++) : "";
            int m = Integer.parseInt(lines.get(idx++).trim());
            String p = m > 0 ? lines.get(idx++) : "";
            if (tc > 0) out.append('\n');
            out.append(isMatch(s, p) ? "true" : "false");
        }
        System.out.print(out);
    }
}
