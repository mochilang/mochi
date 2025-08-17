public class Main {

    static double exp_approx(double x) {
        double term = (double)(1.0);
        double sum_1 = (double)(1.0);
        long i_1 = 1L;
        while ((long)(i_1) < 20L) {
            term = (double)((double)((double)(term) * (double)(x)) / (double)((((Number)(i_1)).doubleValue())));
            sum_1 = (double)((double)(sum_1) + (double)(term));
            i_1 = (long)((long)(i_1) + 1L);
        }
        return sum_1;
    }

    static double[] softmax(double[] vec) {
        double[] exps = ((double[])(new double[]{}));
        long i_3 = 0L;
        while ((long)(i_3) < (long)(vec.length)) {
            exps = ((double[])(appendDouble(exps, (double)(exp_approx((double)(vec[(int)((long)(i_3))]))))));
            i_3 = (long)((long)(i_3) + 1L);
        }
        double total_1 = (double)(0.0);
        i_3 = 0L;
        while ((long)(i_3) < (long)(exps.length)) {
            total_1 = (double)((double)(total_1) + (double)(exps[(int)((long)(i_3))]));
            i_3 = (long)((long)(i_3) + 1L);
        }
        double[] result_1 = ((double[])(new double[]{}));
        i_3 = 0L;
        while ((long)(i_3) < (long)(exps.length)) {
            result_1 = ((double[])(appendDouble(result_1, (double)((double)(exps[(int)((long)(i_3))]) / (double)(total_1)))));
            i_3 = (long)((long)(i_3) + 1L);
        }
        return result_1;
    }

    static double abs_val(double x) {
        if ((double)(x) < (double)(0.0)) {
            return -x;
        }
        return x;
    }

    static boolean approx_equal(double a, double b) {
        return (double)(abs_val((double)((double)(a) - (double)(b)))) < (double)(0.0001);
    }

    static void test_softmax() {
        double[] s1 = ((double[])(softmax(((double[])(new double[]{1.0, 2.0, 3.0, 4.0})))));
        double sum1_1 = (double)(0.0);
        long i_5 = 0L;
        while ((long)(i_5) < (long)(s1.length)) {
            sum1_1 = (double)((double)(sum1_1) + (double)(s1[(int)((long)(i_5))]));
            i_5 = (long)((long)(i_5) + 1L);
        }
        if (!(Boolean)approx_equal((double)(sum1_1), (double)(1.0))) {
            throw new RuntimeException(String.valueOf("sum test failed"));
        }
        double[] s2_1 = ((double[])(softmax(((double[])(new double[]{5.0, 5.0})))));
        if (!(approx_equal((double)(s2_1[(int)(0L)]), (double)(0.5)) && approx_equal((double)(s2_1[(int)(1L)]), (double)(0.5)))) {
            throw new RuntimeException(String.valueOf("equal elements test failed"));
        }
        double[] s3_1 = ((double[])(softmax(((double[])(new double[]{0.0})))));
        if (!(Boolean)approx_equal((double)(s3_1[(int)(0L)]), (double)(1.0))) {
            throw new RuntimeException(String.valueOf("zero vector test failed"));
        }
    }

    static void main() {
        test_softmax();
        System.out.println(_p(softmax(((double[])(new double[]{1.0, 2.0, 3.0, 4.0})))));
    }
    public static void main(String[] args) {
        main();
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
