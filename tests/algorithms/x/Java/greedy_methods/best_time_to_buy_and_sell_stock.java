public class Main {

    static int max_profit(int[] prices) {
        if (prices.length == 0) {
            return 0;
        }
        int min_price = prices[0];
        int max_profit = 0;
        int i = 0;
        while (i < prices.length) {
            int price = prices[i];
            if (price < min_price) {
                min_price = price;
            }
            int profit = price - min_price;
            if (profit > max_profit) {
                max_profit = profit;
            }
            i = i + 1;
        }
        return max_profit;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(max_profit(((int[])(new int[]{7, 1, 5, 3, 6, 4}))));
            System.out.println(max_profit(((int[])(new int[]{7, 6, 4, 3, 1}))));
            long _benchDuration = _now() - _benchStart;
            long _benchMemory = _mem() - _benchMem;
            System.out.println("{");
            System.out.println("  \"duration_us\": " + _benchDuration + ",");
            System.out.println("  \"memory_bytes\": " + _benchMemory + ",");
            System.out.println("  \"name\": \"main\"");
            System.out.println("}");
            return;
        }
    }

    static boolean _nowSeeded = false;
    static int _nowSeed;
    static int _now() {
        if (!_nowSeeded) {
            String s = System.getenv("MOCHI_NOW_SEED");
            if (s != null && !s.isEmpty()) {
                try { _nowSeed = Integer.parseInt(s); _nowSeeded = true; } catch (Exception e) {}
            }
        }
        if (_nowSeeded) {
            _nowSeed = (int)((_nowSeed * 1664525L + 1013904223) % 2147483647);
            return _nowSeed;
        }
        return (int)(System.nanoTime() / 1000);
    }

    static long _mem() {
        Runtime rt = Runtime.getRuntime();
        rt.gc();
        return rt.totalMemory() - rt.freeMemory();
    }
}
