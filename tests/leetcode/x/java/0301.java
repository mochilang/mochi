import java.io.*;
import java.util.*;

public class Main {
    static List<String> solve(String s) {
        int leftRemove = 0, rightRemove = 0;
        for (int i = 0; i < s.length(); i++) {
            char ch = s.charAt(i);
            if (ch == '(') leftRemove++;
            else if (ch == ')') {
                if (leftRemove > 0) leftRemove--;
                else rightRemove++;
            }
        }
        TreeSet<String> ans = new TreeSet<>();
        StringBuilder path = new StringBuilder();
        dfs(s, 0, leftRemove, rightRemove, 0, path, ans);
        return new ArrayList<>(ans);
    }

    static void dfs(String s, int i, int left, int right, int balance, StringBuilder path, Set<String> ans) {
        if (i == s.length()) {
            if (left == 0 && right == 0 && balance == 0) ans.add(path.toString());
            return;
        }
        char ch = s.charAt(i);
        int len = path.length();
        if (ch == '(') {
            if (left > 0) dfs(s, i + 1, left - 1, right, balance, path, ans);
            path.append(ch);
            dfs(s, i + 1, left, right, balance + 1, path, ans);
            path.setLength(len);
        } else if (ch == ')') {
            if (right > 0) dfs(s, i + 1, left, right - 1, balance, path, ans);
            if (balance > 0) {
                path.append(ch);
                dfs(s, i + 1, left, right, balance - 1, path, ans);
                path.setLength(len);
            }
        } else {
            path.append(ch);
            dfs(s, i + 1, left, right, balance, path, ans);
            path.setLength(len);
        }
    }

    public static void main(String[] args) throws Exception {
        BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        String first = br.readLine();
        if (first == null || first.trim().isEmpty()) return;
        int t = Integer.parseInt(first.trim());
        StringBuilder out = new StringBuilder();
        for (int tc = 0; tc < t; tc++) {
            String s = br.readLine();
            List<String> ans = solve(s);
            if (tc > 0) out.append("\n\n");
            out.append(ans.size());
            for (String x : ans) out.append('\n').append(x);
        }
        System.out.print(out);
    }
}
