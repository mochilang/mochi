public class Main {

    static long ugly_numbers(long n) {
        if ((long)(n) <= 0L) {
            return 1;
        }
        long[] ugly_nums_1 = ((long[])(new long[]{}));
        ugly_nums_1 = ((long[])(appendLong(ugly_nums_1, 1L)));
        long i2_1 = 0L;
        long i3_1 = 0L;
        long i5_1 = 0L;
        long next_2_1 = 2L;
        long next_3_1 = 3L;
        long next_5_1 = 5L;
        long count_1 = 1L;
        while ((long)(count_1) < (long)(n)) {
            long next_num_1 = (long)((long)(next_2_1) < (long)(next_3_1) ? (long)(next_2_1) < (long)(next_5_1) ? next_2_1 : next_5_1 : (long)(next_3_1) < (long)(next_5_1) ? next_3_1 : next_5_1);
            ugly_nums_1 = ((long[])(appendLong(ugly_nums_1, (long)(next_num_1))));
            if ((long)(next_num_1) == (long)(next_2_1)) {
                i2_1 = (long)((long)(i2_1) + 1L);
                next_2_1 = (long)((long)(ugly_nums_1[(int)((long)(i2_1))]) * 2L);
            }
            if ((long)(next_num_1) == (long)(next_3_1)) {
                i3_1 = (long)((long)(i3_1) + 1L);
                next_3_1 = (long)((long)(ugly_nums_1[(int)((long)(i3_1))]) * 3L);
            }
            if ((long)(next_num_1) == (long)(next_5_1)) {
                i5_1 = (long)((long)(i5_1) + 1L);
                next_5_1 = (long)((long)(ugly_nums_1[(int)((long)(i5_1))]) * 5L);
            }
            count_1 = (long)((long)(count_1) + 1L);
        }
        return ugly_nums_1[(int)((long)((long)(ugly_nums_1.length) - 1L))];
    }
    public static void main(String[] args) {
        System.out.println(_p(ugly_numbers(100L)));
        System.out.println(_p(ugly_numbers(0L)));
        System.out.println(_p(ugly_numbers(20L)));
        System.out.println(_p(ugly_numbers((long)(-5))));
        System.out.println(_p(ugly_numbers(200L)));
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
