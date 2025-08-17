public class Main {

    static long binomial_coefficient(long total_elements, long elements_to_choose) {
        if ((long)(elements_to_choose) == 0L || (long)(elements_to_choose) == (long)(total_elements)) {
            return 1;
        }
        long k_1 = (long)(elements_to_choose);
        if ((long)(k_1) > (long)((long)(total_elements) - (long)(k_1))) {
            k_1 = (long)((long)(total_elements) - (long)(k_1));
        }
        long coefficient_1 = 1L;
        long i_1 = 0L;
        while ((long)(i_1) < (long)(k_1)) {
            coefficient_1 = (long)((long)(coefficient_1) * (long)(((long)(total_elements) - (long)(i_1))));
            coefficient_1 = Math.floorDiv(coefficient_1, ((long)(i_1) + 1L));
            i_1 = (long)((long)(i_1) + 1L);
        }
        return coefficient_1;
    }

    static long[] bell_numbers(long max_set_length) {
        if ((long)(max_set_length) < 0L) {
            throw new RuntimeException(String.valueOf("max_set_length must be non-negative"));
        }
        long[] bell_1 = ((long[])(new long[]{}));
        long i_3 = 0L;
        while ((long)(i_3) <= (long)(max_set_length)) {
            bell_1 = ((long[])(appendLong(bell_1, 0L)));
            i_3 = (long)((long)(i_3) + 1L);
        }
bell_1[(int)(0L)] = 1L;
        i_3 = 1L;
        while ((long)(i_3) <= (long)(max_set_length)) {
            long j_1 = 0L;
            while ((long)(j_1) < (long)(i_3)) {
bell_1[(int)((long)(i_3))] = (long)((long)(bell_1[(int)((long)(i_3))]) + (long)((long)(binomial_coefficient((long)((long)(i_3) - 1L), (long)(j_1))) * (long)(bell_1[(int)((long)(j_1))])));
                j_1 = (long)((long)(j_1) + 1L);
            }
            i_3 = (long)((long)(i_3) + 1L);
        }
        return bell_1;
    }

    static void main() {
        System.out.println(_p(bell_numbers(5L)));
    }
    public static void main(String[] args) {
        main();
    }

    static long[] appendLong(long[] arr, long v) {
        long[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
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
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
