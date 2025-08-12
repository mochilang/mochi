public class Main {

    static long max_profit(long[] prices) {
        if (prices.length == 0) {
            return 0;
        }
        long min_price_1 = prices[(int)((long)(0))];
        long max_profit_1 = 0L;
        long i_1 = 0L;
        while (i_1 < prices.length) {
            long price_1 = prices[(int)((long)(i_1))];
            if (price_1 < min_price_1) {
                min_price_1 = price_1;
            }
            long profit_1 = price_1 - min_price_1;
            if (profit_1 > max_profit_1) {
                max_profit_1 = profit_1;
            }
            i_1 = i_1 + 1;
        }
        return max_profit_1;
    }
    public static void main(String[] args) {
        System.out.println(max_profit(((long[])(new long[]{7, 1, 5, 3, 6, 4}))));
        System.out.println(max_profit(((long[])(new long[]{7, 6, 4, 3, 1}))));
    }
}
