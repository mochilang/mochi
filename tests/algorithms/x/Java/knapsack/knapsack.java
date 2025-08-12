public class Main {

    static long knapsack(long capacity, long[] weights, long[] values, long counter) {
        if (counter == 0 || capacity == 0) {
            return 0;
        }
        if (weights[(int)((long)(counter - 1))] > capacity) {
            return knapsack(capacity, ((long[])(weights)), ((long[])(values)), counter - 1);
        } else {
            long left_capacity_1 = capacity - weights[(int)((long)(counter - 1))];
            long new_value_included_1 = values[(int)((long)(counter - 1))] + knapsack(left_capacity_1, ((long[])(weights)), ((long[])(values)), counter - 1);
            long without_new_value_1 = knapsack(capacity, ((long[])(weights)), ((long[])(values)), counter - 1);
            if (new_value_included_1 > without_new_value_1) {
                return new_value_included_1;
            } else {
                return without_new_value_1;
            }
        }
    }

    static void main() {
        long[] weights = ((long[])(new long[]{10, 20, 30}));
        long[] values_1 = ((long[])(new long[]{60, 100, 120}));
        long cap_1 = 50L;
        long count_1 = values_1.length;
        long result_1 = knapsack(cap_1, ((long[])(weights)), ((long[])(values_1)), count_1);
        System.out.println(_p(result_1));
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
