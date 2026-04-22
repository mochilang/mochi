import java.io.*;
import java.util.*;

public class Main {
    static int best;

    static int dfs(int i, int[] vals, boolean[] ok) {
        if (i >= vals.length || !ok[i]) return 0;
        int left = Math.max(0, dfs(2 * i + 1, vals, ok));
        int right = Math.max(0, dfs(2 * i + 2, vals, ok));
        best = Math.max(best, vals[i] + left + right);
        return vals[i] + Math.max(left, right);
    }

    static int solve(int[] vals, boolean[] ok) {
        best = -1000000000;
        dfs(0, vals, ok);
        return best;
    }

    public static void main(String[] args) throws Exception {
        List<String> lines = Arrays.asList(new String(System.in.readAllBytes()).split("\\R", -1));
        if (lines.isEmpty() || lines.get(0).trim().isEmpty()) return;
        int tc = Integer.parseInt(lines.get(0).trim());
        int idx = 1;
        StringBuilder out = new StringBuilder();
        for (int t = 0; t < tc; t++) {
            int n = Integer.parseInt(lines.get(idx++).trim());
            int[] vals = new int[n];
            boolean[] ok = new boolean[n];
            for (int i = 0; i < n; i++) {
                String tok = lines.get(idx++).trim();
                if (!tok.equals("null")) { ok[i] = true; vals[i] = Integer.parseInt(tok); }
            }
            if (t > 0) out.append('\n');
            out.append(solve(vals, ok));
        }
        System.out.print(out);
    }
}
