import java.io.*;
import java.util.*;

public class Main {
    static int solve(int[] a) {
        int best = 0;
        for (int i = 0; i < a.length; i++) {
            int mn = a[i];
            for (int j = i; j < a.length; j++) {
                if (a[j] < mn) mn = a[j];
                int area = mn * (j - i + 1);
                if (area > best) best = area;
            }
        }
        return best;
    }

    public static void main(String[] args) throws Exception {
        String data = new String(System.in.readAllBytes());
        String[] toks = data.trim().isEmpty() ? new String[0] : data.trim().split("\\s+");
        if (toks.length == 0) return;
        int idx = 0, t = Integer.parseInt(toks[idx++]);
        StringBuilder out = new StringBuilder();
        for (int tc = 0; tc < t; tc++) {
            int n = Integer.parseInt(toks[idx++]);
            int[] a = new int[n];
            for (int i = 0; i < n; i++) a[i] = Integer.parseInt(toks[idx++]);
            if (tc > 0) out.append('\n');
            out.append(solve(a));
        }
        System.out.print(out);
    }
}
