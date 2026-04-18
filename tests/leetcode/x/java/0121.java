import java.io.*;
import java.util.*;

public class Main {
    static int maxProfit(int[] prices) {
        if (prices.length == 0) return 0;
        int minPrice = prices[0], best = 0;
        for (int i = 1; i < prices.length; i++) {
            best = Math.max(best, prices[i] - minPrice);
            minPrice = Math.min(minPrice, prices[i]);
        }
        return best;
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
