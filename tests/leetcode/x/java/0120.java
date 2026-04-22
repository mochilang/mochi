import java.io.*;
import java.util.*;

public class Main {
    static int solve(List<List<Integer>> tri) {
        ArrayList<Integer> dp = new ArrayList<>(tri.get(tri.size() - 1));
        for (int i = tri.size() - 2; i >= 0; i--) {
            for (int j = 0; j <= i; j++) dp.set(j, tri.get(i).get(j) + Math.min(dp.get(j), dp.get(j + 1)));
        }
        return dp.get(0);
    }

    public static void main(String[] args) throws Exception {
        String[] toks = new String(System.in.readAllBytes()).trim().split("\\s+");
        if (toks.length == 0 || toks[0].isEmpty()) return;
        int idx = 0, t = Integer.parseInt(toks[idx++]);
        StringBuilder out = new StringBuilder();
        for (int tc = 0; tc < t; tc++) {
            int rows = Integer.parseInt(toks[idx++]);
            List<List<Integer>> tri = new ArrayList<>();
            for (int r = 1; r <= rows; r++) {
                List<Integer> row = new ArrayList<>();
                for (int j = 0; j < r; j++) row.add(Integer.parseInt(toks[idx++]));
                tri.add(row);
            }
            if (tc > 0) out.append('\n');
            out.append(solve(tri));
        }
        System.out.print(out);
    }
}
