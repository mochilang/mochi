public class Main {

    static boolean perfect(long n) {
        if ((long)(n) <= 0L) {
            return false;
        }
        long limit_1 = Math.floorDiv(((long)(n)), ((long)(2)));
        long sum_1 = 0L;
        long i_1 = 1L;
        while ((long)(i_1) <= (long)(limit_1)) {
            if (Math.floorMod(n, i_1) == 0L) {
                sum_1 = (long)((long)(sum_1) + (long)(i_1));
            }
            i_1 = (long)((long)(i_1) + 1L);
        }
        return (long)(sum_1) == (long)(n);
    }

    static void main() {
        long[] numbers = ((long[])(new long[]{6, 28, 29, 12, 496, 8128, 0, -1}));
        long idx_1 = 0L;
        while ((long)(idx_1) < (long)(numbers.length)) {
            long num_1 = (long)(numbers[(int)((long)(idx_1))]);
            if (perfect((long)(num_1))) {
                System.out.println(_p(num_1) + " is a Perfect Number.");
            } else {
                System.out.println(_p(num_1) + " is not a Perfect Number.");
            }
            idx_1 = (long)((long)(idx_1) + 1L);
        }
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
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
