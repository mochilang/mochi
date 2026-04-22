import java.util.*;

public class Main {
    static String solve(String s) {
        String rev = new StringBuilder(s).reverse().toString();
        String combined = s + "#" + rev;
        int[] pi = new int[combined.length()];
        for (int i = 1; i < combined.length(); i++) {
            int j = pi[i - 1];
            while (j > 0 && combined.charAt(i) != combined.charAt(j)) j = pi[j - 1];
            if (combined.charAt(i) == combined.charAt(j)) j++;
            pi[i] = j;
        }
        int keep = pi.length == 0 ? 0 : pi[pi.length - 1];
        return rev.substring(0, s.length() - keep) + s;
    }

    public static void main(String[] args) throws Exception {
        List<String> lines = Arrays.asList(new String(System.in.readAllBytes()).split("\\R", -1));
        if (lines.isEmpty() || lines.get(0).trim().isEmpty()) return;
        int t = Integer.parseInt(lines.get(0).trim());
        StringBuilder out = new StringBuilder();
        for (int i = 0; i < t; i++) {
            if (i > 0) out.append('\n');
            String s = i + 1 < lines.size() ? lines.get(i + 1) : "";
            out.append(solve(s));
        }
        System.out.print(out);
    }
}
