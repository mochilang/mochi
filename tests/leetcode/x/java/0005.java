import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;

public class Main {
    private static int[] expand(String s, int left, int right) {
        while (left >= 0 && right < s.length() && s.charAt(left) == s.charAt(right)) {
            left--;
            right++;
        }
        return new int[] {left + 1, right - left - 1};
    }

    private static String longestPalindrome(String s) {
        int bestStart = 0;
        int bestLen = s.isEmpty() ? 0 : 1;
        for (int i = 0; i < s.length(); i++) {
            int[] odd = expand(s, i, i);
            if (odd[1] > bestLen) {
                bestStart = odd[0];
                bestLen = odd[1];
            }
            int[] even = expand(s, i, i + 1);
            if (even[1] > bestLen) {
                bestStart = even[0];
                bestLen = even[1];
            }
        }
        return s.substring(bestStart, bestStart + bestLen);
    }

    public static void main(String[] args) throws Exception {
        BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        String first = br.readLine();
        if (first == null) return;
        int t = Integer.parseInt(first.trim());
        List<String> out = new ArrayList<>();
        for (int i = 0; i < t; i++) {
            String s = br.readLine();
            if (s == null) s = "";
            out.add(longestPalindrome(s));
        }
        System.out.print(String.join("\n", out));
    }
}
