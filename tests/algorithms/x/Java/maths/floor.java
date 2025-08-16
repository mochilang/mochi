public class Main {

    static long floor(double x) {
        long i = (long)(((Number)(x)).intValue());
        if ((double)((double)(x) - (double)(((Number)(i)).doubleValue())) >= (double)(0.0)) {
            return i;
        }
        return (long)(i) - 1L;
    }

    static void test_floor() {
        double[] nums = ((double[])(new double[]{1.0, -1.0, 0.0, 0.0, 1.1, -1.1, 1.0, -1.0, 1000000000.0}));
        long[] expected_1 = ((long[])(new long[]{1, -1, 0, 0, 1, -2, 1, -1, 1000000000}));
        long idx_1 = 0L;
        while ((long)(idx_1) < (long)(nums.length)) {
            if (Math.floor(nums[(int)((long)(idx_1))]) != (long)(expected_1[(int)((long)(idx_1))])) {
                throw new RuntimeException(String.valueOf("floor test failed"));
            }
            idx_1 = (long)((long)(idx_1) + 1L);
        }
    }

    static void main() {
        test_floor();
        System.out.println(_p(Math.floor(-1.1)));
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
