import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class Main {
    private static List<String> solve(String num, long target) {
        List<String> ans = new ArrayList<>();
        dfs(num, target, 0, "", 0, 0, ans);
        Collections.sort(ans);
        return ans;
    }

    private static void dfs(String num, long target, int i, String expr, long value, long last, List<String> ans) {
        if (i == num.length()) {
            if (value == target) ans.add(expr);
            return;
        }
        for (int j = i; j < num.length(); j++) {
            if (j > i && num.charAt(i) == '0') break;
            String s = num.substring(i, j + 1);
            long n = Long.parseLong(s);
            if (i == 0) {
                dfs(num, target, j + 1, s, n, n, ans);
            } else {
                dfs(num, target, j + 1, expr + "+" + s, value + n, n, ans);
                dfs(num, target, j + 1, expr + "-" + s, value - n, -n, ans);
                dfs(num, target, j + 1, expr + "*" + s, value - last + last * n, last * n, ans);
            }
        }
    }

    public static void main(String[] args) throws Exception {
        BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
        String first = reader.readLine();
        if (first == null) return;
        int t = Integer.parseInt(first.trim());
        StringBuilder out = new StringBuilder();
        for (int tc = 0; tc < t; tc++) {
            String num = reader.readLine().trim();
            long target = Long.parseLong(reader.readLine().trim());
            List<String> ans = solve(num, target);
            if (tc > 0) out.append("\n\n");
            out.append(ans.size());
            for (String s : ans) out.append('\n').append(s);
        }
        System.out.print(out);
    }
}
