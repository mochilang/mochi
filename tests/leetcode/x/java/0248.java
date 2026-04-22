import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;

public class Main {
    private static final char[][] PAIRS = {{'0', '0'}, {'1', '1'}, {'6', '9'}, {'8', '8'}, {'9', '6'}};

    private static List<String> build(int n, int m) {
        if (n == 0) return List.of("");
        if (n == 1) return List.of("0", "1", "8");
        List<String> mids = build(n - 2, m);
        List<String> res = new ArrayList<>();
        for (String mid : mids) {
            for (char[] p : PAIRS) {
                if (n == m && p[0] == '0') continue;
                res.add("" + p[0] + mid + p[1]);
            }
        }
        return res;
    }

    private static int countRange(String low, String high) {
        int ans = 0;
        for (int len = low.length(); len <= high.length(); len++) {
            for (String s : build(len, len)) {
                if (len == low.length() && s.compareTo(low) < 0) continue;
                if (len == high.length() && s.compareTo(high) > 0) continue;
                ans++;
            }
        }
        return ans;
    }

    public static void main(String[] args) throws Exception {
        BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
        String first = reader.readLine();
        if (first == null) return;
        int t = Integer.parseInt(first.trim());
        StringBuilder out = new StringBuilder();
        for (int i = 0; i < t; i++) {
            String low = reader.readLine().trim();
            String high = reader.readLine().trim();
            if (i > 0) out.append('\n');
            out.append(countRange(low, high));
        }
        System.out.print(out);
    }
}
