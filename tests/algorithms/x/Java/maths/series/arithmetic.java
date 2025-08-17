public class Main {

    static boolean is_arithmetic_series(double[] xs) {
        if ((long)(xs.length) == 0L) {
            throw new RuntimeException(String.valueOf("Input list must be a non empty list"));
        }
        if ((long)(xs.length) == 1L) {
            return true;
        }
        double diff_1 = (double)((double)(xs[(int)(1L)]) - (double)(xs[(int)(0L)]));
        long i_1 = 0L;
        while ((long)(i_1) < (long)((long)(xs.length) - 1L)) {
            if ((double)((double)(xs[(int)((long)((long)(i_1) + 1L))]) - (double)(xs[(int)((long)(i_1))])) != (double)(diff_1)) {
                return false;
            }
            i_1 = (long)((long)(i_1) + 1L);
        }
        return true;
    }

    static double arithmetic_mean(double[] xs) {
        if ((long)(xs.length) == 0L) {
            throw new RuntimeException(String.valueOf("Input list must be a non empty list"));
        }
        double total_1 = (double)(0.0);
        long i_3 = 0L;
        while ((long)(i_3) < (long)(xs.length)) {
            total_1 = (double)((double)(total_1) + (double)(xs[(int)((long)(i_3))]));
            i_3 = (long)((long)(i_3) + 1L);
        }
        return (double)(total_1) / (double)((((Number)(xs.length)).doubleValue()));
    }
    public static void main(String[] args) {
        System.out.println(_p(is_arithmetic_series(((double[])(new double[]{2.0, 4.0, 6.0})))));
        System.out.println(_p(is_arithmetic_series(((double[])(new double[]{3.0, 6.0, 12.0, 24.0})))));
        System.out.println(_p(arithmetic_mean(((double[])(new double[]{2.0, 4.0, 6.0})))));
        System.out.println(_p(arithmetic_mean(((double[])(new double[]{3.0, 6.0, 9.0, 12.0})))));
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
