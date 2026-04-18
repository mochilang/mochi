import java.io.*;
import java.util.*;

public class Main {
    static int myAtoi(String s) {
        int i = 0;
        while (i < s.length() && s.charAt(i) == ' ') i++;
        int sign = 1;
        if (i < s.length() && (s.charAt(i) == '+' || s.charAt(i) == '-')) {
            if (s.charAt(i) == '-') sign = -1;
            i++;
        }
        int ans = 0;
        int limit = sign > 0 ? 7 : 8;
        while (i < s.length() && Character.isDigit(s.charAt(i))) {
            int digit = s.charAt(i) - '0';
            if (ans > 214748364 || (ans == 214748364 && digit > limit)) return sign > 0 ? Integer.MAX_VALUE : Integer.MIN_VALUE;
            ans = ans * 10 + digit;
            i++;
        }
        return sign * ans;
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
            out.add(Integer.toString(myAtoi(s)));
        }
        System.out.print(String.join("\n", out));
    }
}
