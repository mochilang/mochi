import java.io.BufferedReader;
import java.io.InputStreamReader;

public class Main {
    private static long countDigitOne(long n) {
        long total = 0;
        for (long m = 1; m <= n; m *= 10) {
            long high = n / (m * 10);
            long cur = (n / m) % 10;
            long low = n % m;
            if (cur == 0) total += high * m;
            else if (cur == 1) total += high * m + low + 1;
            else total += (high + 1) * m;
        }
        return total;
    }

    public static void main(String[] args) throws Exception {
        BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
        String first = reader.readLine();
        if (first == null) return;
        int t = Integer.parseInt(first.trim());
        StringBuilder out = new StringBuilder();
        for (int i = 0; i < t; i++) {
            if (i > 0) out.append('\n');
            out.append(countDigitOne(Long.parseLong(reader.readLine().trim())));
        }
        System.out.print(out);
    }
}
