public class Main {

    static long knapsack(long[] weights, long[] values, long number_of_items, long max_weight, long index) {
        if (index == number_of_items) {
            return 0;
        }
        long ans1_1 = knapsack(((long[])(weights)), ((long[])(values)), number_of_items, max_weight, index + 1);
        long ans2_1 = 0L;
        if (weights[(int)((long)(index))] <= max_weight) {
            ans2_1 = values[(int)((long)(index))] + knapsack(((long[])(weights)), ((long[])(values)), number_of_items, max_weight - weights[(int)((long)(index))], index + 1);
        }
        if (ans1_1 > ans2_1) {
            return ans1_1;
        }
        return ans2_1;
    }

    static void main() {
        long[] w1 = ((long[])(new long[]{1, 2, 4, 5}));
        long[] v1_1 = ((long[])(new long[]{5, 4, 8, 6}));
        long r1_1 = knapsack(((long[])(w1)), ((long[])(v1_1)), 4L, 5L, 0L);
        System.out.println(_p(r1_1));
        long[] w2_1 = ((long[])(new long[]{3, 4, 5}));
        long[] v2_1 = ((long[])(new long[]{10, 9, 8}));
        long r2_1 = knapsack(((long[])(w2_1)), ((long[])(v2_1)), 3L, 25L, 0L);
        System.out.println(_p(r2_1));
    }
    public static void main(String[] args) {
        main();
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
        if (v instanceof Double || v instanceof Float) {
            double d = ((Number) v).doubleValue();
            if (d == Math.rint(d)) return String.valueOf((long) d);
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
