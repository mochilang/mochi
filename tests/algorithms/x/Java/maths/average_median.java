public class Main {

    static long[] bubble_sort(long[] nums) {
        long[] arr = ((long[])(nums));
        long n_1 = (long)(arr.length);
        long i_1 = 0L;
        while ((long)(i_1) < (long)(n_1)) {
            long j_1 = 0L;
            while ((long)(j_1) < (long)((long)(n_1) - 1L)) {
                long a_1 = (long)(arr[(int)((long)(j_1))]);
                long b_1 = (long)(arr[(int)((long)((long)(j_1) + 1L))]);
                if ((long)(a_1) > (long)(b_1)) {
arr[(int)((long)(j_1))] = (long)(b_1);
arr[(int)((long)((long)(j_1) + 1L))] = (long)(a_1);
                }
                j_1 = (long)((long)(j_1) + 1L);
            }
            i_1 = (long)((long)(i_1) + 1L);
        }
        return arr;
    }

    static double median(long[] nums) {
        long[] sorted_list = ((long[])(bubble_sort(((long[])(nums)))));
        long length_1 = (long)(sorted_list.length);
        long mid_index_1 = Math.floorDiv(length_1, 2);
        if (Math.floorMod(length_1, 2) == 0L) {
            return (double)((((Number)(((long)(sorted_list[(int)((long)(mid_index_1))]) + (long)(sorted_list[(int)((long)((long)(mid_index_1) - 1L))])))).doubleValue())) / (double)(2.0);
        } else {
            return ((double)(sorted_list[(int)((long)(mid_index_1))]));
        }
    }
    public static void main(String[] args) {
        System.out.println(_p(median(((long[])(new long[]{0})))));
        System.out.println(_p(median(((long[])(new long[]{4, 1, 3, 2})))));
        System.out.println(_p(median(((long[])(new long[]{2, 70, 6, 50, 20, 8, 4})))));
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
