import java.io.*;
import java.util.*;

public class Main {
    static String getPermutation(int n, int k) {
        List<String> digits = new ArrayList<>();
        for (int i = 1; i <= n; i++) digits.add(String.valueOf(i));
        int[] fact = new int[n + 1];
        fact[0] = 1;
        for (int i = 1; i <= n; i++) fact[i] = fact[i - 1] * i;
        k--;
        StringBuilder out = new StringBuilder();
        for (int rem = n; rem >= 1; rem--) {
            int block = fact[rem - 1];
            int idx = k / block;
            k %= block;
            out.append(digits.remove(idx));
        }
        return out.toString();
    }

    public static void main(String[] args) throws Exception {
        BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        List<String> lines = new ArrayList<>();
        String line;
        while ((line = br.readLine()) != null) lines.add(line.trim());
        if (lines.isEmpty() || lines.get(0).isEmpty()) return;
        int idx = 0;
        int t = Integer.parseInt(lines.get(idx++));
        List<String> out = new ArrayList<>();
        for (int tc = 0; tc < t; tc++) {
            int n = Integer.parseInt(lines.get(idx++));
            int k = Integer.parseInt(lines.get(idx++));
            out.add(getPermutation(n, k));
        }
        System.out.print(String.join("\n", out));
    }
}
