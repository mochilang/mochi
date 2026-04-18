import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;

public class Main {
    private static int reverseInt(int x) {
        int ans = 0;
        while (x != 0) {
            int digit = x % 10;
            x /= 10;
            if (ans > Integer.MAX_VALUE / 10 || (ans == Integer.MAX_VALUE / 10 && digit > 7)) return 0;
            if (ans < Integer.MIN_VALUE / 10 || (ans == Integer.MIN_VALUE / 10 && digit < -8)) return 0;
            ans = ans * 10 + digit;
        }
        return ans;
    }

    public static void main(String[] args) throws Exception {
        BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        String first = br.readLine();
        if (first == null) return;
        int t = Integer.parseInt(first.trim());
        List<String> out = new ArrayList<>();
        for (int i = 0; i < t; i++) {
            String line = br.readLine();
            int x = line == null ? 0 : Integer.parseInt(line.trim());
            out.add(Integer.toString(reverseInt(x)));
        }
        System.out.print(String.join("\n", out));
    }
}
