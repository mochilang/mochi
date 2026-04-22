import java.util.*;

public class Main {
    static int solve(int k, int[] prices) {
        int n = prices.length;
        if (k >= n / 2) {
            int best = 0;
            for (int i = 1; i < n; i++) if (prices[i] > prices[i - 1]) best += prices[i] - prices[i - 1];
            return best;
        }
        long negInf = -(1L << 60);
        long[] buy = new long[k + 1];
        long[] sell = new long[k + 1];
        Arrays.fill(buy, negInf);
        buy[0] = negInf;
        for (int price : prices) {
            for (int t = 1; t <= k; t++) {
                buy[t] = Math.max(buy[t], sell[t - 1] - price);
                sell[t] = Math.max(sell[t], buy[t] + price);
            }
        }
        return (int) sell[k];
    }

    public static void main(String[] args) throws Exception {
        String input = new String(System.in.readAllBytes()).trim();
        if (input.isEmpty()) return;
        String[] toks = input.split("\\s+");
        int idx = 0, t = Integer.parseInt(toks[idx++]);
        StringBuilder out = new StringBuilder();
        for (int tc = 0; tc < t; tc++) {
            int k = Integer.parseInt(toks[idx++]);
            int n = Integer.parseInt(toks[idx++]);
            int[] prices = new int[n];
            for (int i = 0; i < n; i++) prices[i] = Integer.parseInt(toks[idx++]);
            if (tc > 0) out.append('\n');
            out.append(solve(k, prices));
        }
        System.out.print(out);
    }
}
