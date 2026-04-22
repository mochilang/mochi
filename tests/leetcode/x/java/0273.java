import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;

public class Main {
    private static final String[] LESS20 = {"", "One", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine",
            "Ten", "Eleven", "Twelve", "Thirteen", "Fourteen", "Fifteen", "Sixteen", "Seventeen", "Eighteen", "Nineteen"};
    private static final String[] TENS = {"", "", "Twenty", "Thirty", "Forty", "Fifty", "Sixty", "Seventy", "Eighty", "Ninety"};
    private static final String[] THOUSANDS = {"", "Thousand", "Million", "Billion"};

    private static String helper(int n) {
        if (n == 0) return "";
        if (n < 20) return LESS20[n];
        if (n < 100) return TENS[n / 10] + (n % 10 == 0 ? "" : " " + helper(n % 10));
        return LESS20[n / 100] + " Hundred" + (n % 100 == 0 ? "" : " " + helper(n % 100));
    }

    private static String solve(int num) {
        if (num == 0) return "Zero";
        List<String> parts = new ArrayList<>();
        int idx = 0;
        while (num > 0) {
            int chunk = num % 1000;
            if (chunk != 0) {
                String words = helper(chunk);
                if (!THOUSANDS[idx].isEmpty()) words += " " + THOUSANDS[idx];
                parts.add(words);
            }
            num /= 1000;
            idx++;
        }
        StringBuilder out = new StringBuilder();
        for (int i = parts.size() - 1; i >= 0; i--) {
            if (out.length() > 0) out.append(' ');
            out.append(parts.get(i));
        }
        return out.toString();
    }

    public static void main(String[] args) throws Exception {
        BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
        String first = reader.readLine();
        if (first == null) return;
        int t = Integer.parseInt(first.trim());
        StringBuilder out = new StringBuilder();
        for (int i = 0; i < t; i++) {
            if (i > 0) out.append('\n');
            out.append(solve(Integer.parseInt(reader.readLine().trim())));
        }
        System.out.print(out);
    }
}
