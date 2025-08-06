public class Main {

    static int[] make_list(int len, int value) {
        int[] arr = ((int[])(new int[]{}));
        int i = 0;
        while (i < len) {
            arr = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(arr), java.util.stream.IntStream.of(value)).toArray()));
            i = i + 1;
        }
        return arr;
    }

    static int max_int(int a, int b) {
        if (a > b) {
            return a;
        } else {
            return b;
        }
    }

    static int min_int(int a, int b) {
        if (a < b) {
            return a;
        } else {
            return b;
        }
    }

    static int min3(int a, int b, int c) {
        return min_int(min_int(a, b), c);
    }

    static int minimum_tickets_cost(int[] days, int[] costs) {
        if (days.length == 0) {
            return 0;
        }
        int last_day = days[days.length - 1];
        int[] dp = ((int[])(make_list(last_day + 1, 0)));
        int day_index = 0;
        int d = 1;
        while (d <= last_day) {
            if (day_index < days.length && d == days[day_index]) {
                int cost1 = dp[d - 1] + costs[0];
                int cost7 = dp[max_int(0, d - 7)] + costs[1];
                int cost30 = dp[max_int(0, d - 30)] + costs[2];
dp[d] = min3(cost1, cost7, cost30);
                day_index = day_index + 1;
            } else {
dp[d] = dp[d - 1];
            }
            d = d + 1;
        }
        return dp[last_day];
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(_p(minimum_tickets_cost(((int[])(new int[]{1, 4, 6, 7, 8, 20})), ((int[])(new int[]{2, 7, 15})))));
            System.out.println(_p(minimum_tickets_cost(((int[])(new int[]{1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 30, 31})), ((int[])(new int[]{2, 7, 15})))));
            System.out.println(_p(minimum_tickets_cost(((int[])(new int[]{1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 30, 31})), ((int[])(new int[]{2, 90, 150})))));
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

    static String _p(Object v) {
        if (v == null) return "<nil>";
        if (v.getClass().isArray()) {
            if (v instanceof int[]) return java.util.Arrays.toString((int[]) v);
            if (v instanceof long[]) return java.util.Arrays.toString((long[]) v);
            if (v instanceof double[]) return java.util.Arrays.toString((double[]) v);
            if (v instanceof boolean[]) return java.util.Arrays.toString((boolean[]) v);
            if (v instanceof byte[]) return java.util.Arrays.toString((byte[]) v);
            if (v instanceof char[]) return java.util.Arrays.toString((char[]) v);
            if (v instanceof short[]) return java.util.Arrays.toString((short[]) v);
            if (v instanceof float[]) return java.util.Arrays.toString((float[]) v);
            return java.util.Arrays.deepToString((Object[]) v);
        }
        return String.valueOf(v);
    }
}
