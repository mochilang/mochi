public class Main {

    static double exp_approx(double x) {
        double sum = (double)(1.0);
        double term_1 = (double)(1.0);
        long i_1 = 1L;
        while ((long)(i_1) <= 10L) {
            term_1 = (double)((double)((double)(term_1) * (double)(x)) / (double)((((Number)(i_1)).doubleValue())));
            sum = (double)((double)(sum) + (double)(term_1));
            i_1 = (long)((long)(i_1) + 1L);
        }
        return sum;
    }

    static double[] sigmoid(double[] vector) {
        double[] result = ((double[])(new double[]{}));
        long i_3 = 0L;
        while ((long)(i_3) < (long)(vector.length)) {
            double v_1 = (double)(vector[(int)((long)(i_3))]);
            double s_1 = (double)((double)(1.0) / (double)(((double)(1.0) + (double)(exp_approx((double)(-v_1))))));
            result = ((double[])(appendDouble(result, (double)(s_1))));
            i_3 = (long)((long)(i_3) + 1L);
        }
        return result;
    }
    public static void main(String[] args) {
        System.out.println(_p(sigmoid(((double[])(new double[]{-1.0, 1.0, 2.0})))));
        System.out.println(_p(sigmoid(((double[])(new double[]{0.0})))));
    }

    static double[] appendDouble(double[] arr, double v) {
        double[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
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
