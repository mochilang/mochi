import java.io.*;
import java.util.*;

public class Main {
    static int maxProfit(int[] prices) {
        int buy1 = -1_000_000_000, sell1 = 0, buy2 = -1_000_000_000, sell2 = 0;
        for (int p : prices) {
            buy1 = Math.max(buy1, -p);
            sell1 = Math.max(sell1, buy1 + p);
            buy2 = Math.max(buy2, sell1 - p);
            sell2 = Math.max(sell2, buy2 + p);
        }
        return sell2;
    }

    public static void main(String[] args) throws Exception {
        BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        List<String> lines = new ArrayList<>();
        String line;
        while ((line = br.readLine()) != null) lines.add(line);
        if (lines.isEmpty()) return;
        int t = Integer.parseInt(lines.get(0).trim());
        int idx = 1;
        StringBuilder out = new StringBuilder();
        for (int tc = 0; tc < t; tc++) {
            int n = Integer.parseInt(lines.get(idx++).trim());
            int[] prices = new int[n];
            for (int i = 0; i < n; i++) prices[i] = Integer.parseInt(lines.get(idx++).trim());
            if (tc > 0) out.append('\n');
            out.append(maxProfit(prices));
        }
        System.out.print(out.toString());
    }
}
