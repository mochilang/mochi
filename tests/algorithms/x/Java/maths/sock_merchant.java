public class Main {

    static long sock_merchant(long[] colors) {
        long[] arr = ((long[])(new long[]{}));
        long i_1 = 0L;
        while ((long)(i_1) < (long)(colors.length)) {
            arr = ((long[])(appendLong(arr, (long)(colors[(int)((long)(i_1))]))));
            i_1 = (long)((long)(i_1) + 1L);
        }
        long n_1 = (long)(arr.length);
        long a_1 = 0L;
        while ((long)(a_1) < (long)(n_1)) {
            long min_idx_1 = (long)(a_1);
            long b_1 = (long)((long)(a_1) + 1L);
            while ((long)(b_1) < (long)(n_1)) {
                if ((long)(arr[(int)((long)(b_1))]) < (long)(arr[(int)((long)(min_idx_1))])) {
                    min_idx_1 = (long)(b_1);
                }
                b_1 = (long)((long)(b_1) + 1L);
            }
            long temp_1 = (long)(arr[(int)((long)(a_1))]);
arr[(int)((long)(a_1))] = (long)(arr[(int)((long)(min_idx_1))]);
arr[(int)((long)(min_idx_1))] = (long)(temp_1);
            a_1 = (long)((long)(a_1) + 1L);
        }
        long pairs_1 = 0L;
        i_1 = 0L;
        while ((long)(i_1) < (long)(n_1)) {
            long count_1 = 1L;
            while ((long)((long)(i_1) + 1L) < (long)(n_1) && (long)(arr[(int)((long)(i_1))]) == (long)(arr[(int)((long)((long)(i_1) + 1L))])) {
                count_1 = (long)((long)(count_1) + 1L);
                i_1 = (long)((long)(i_1) + 1L);
            }
            pairs_1 = (long)((long)(pairs_1) + Math.floorDiv(count_1, 2));
            i_1 = (long)((long)(i_1) + 1L);
        }
        return pairs_1;
    }

    static void test_sock_merchant() {
        long[] example1 = ((long[])(new long[]{10, 20, 20, 10, 10, 30, 50, 10, 20}));
        if ((long)(sock_merchant(((long[])(example1)))) != 3L) {
            throw new RuntimeException(String.valueOf("example1 failed"));
        }
        long[] example2_1 = ((long[])(new long[]{1, 1, 3, 3}));
        if ((long)(sock_merchant(((long[])(example2_1)))) != 2L) {
            throw new RuntimeException(String.valueOf("example2 failed"));
        }
    }

    static void main() {
        test_sock_merchant();
        long[] example1_2 = ((long[])(new long[]{10, 20, 20, 10, 10, 30, 50, 10, 20}));
        System.out.println(_p(sock_merchant(((long[])(example1_2)))));
        long[] example2_3 = ((long[])(new long[]{1, 1, 3, 3}));
        System.out.println(_p(sock_merchant(((long[])(example2_3)))));
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
