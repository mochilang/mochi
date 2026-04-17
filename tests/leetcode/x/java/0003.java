import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.HashMap;
import java.util.Map;

public class Main {
    static int longest(String s) {
        Map<Character, Integer> last = new HashMap<>();
        int left = 0;
        int best = 0;
        for (int right = 0; right < s.length(); right++) {
            char ch = s.charAt(right);
            if (last.containsKey(ch) && last.get(ch) >= left) {
                left = last.get(ch) + 1;
            }
            last.put(ch, right);
            best = Math.max(best, right - left + 1);
        }
        return best;
    }

    public static void main(String[] args) throws Exception {
        BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        String first = br.readLine();
        if (first == null || first.isEmpty()) {
            return;
        }
        int t = Integer.parseInt(first.trim());
        StringBuilder out = new StringBuilder();
        for (int i = 0; i < t; i++) {
            String s = br.readLine();
            if (s == null) s = "";
            out.append(longest(s));
            if (i + 1 < t) out.append('\n');
        }
        System.out.print(out);
    }
}
