public class Main {

    static double exp_approx(double x) {
        double sum = 1.0;
        double term_1 = 1.0;
        long i_1 = 1L;
        while ((long)(i_1) <= (long)(20)) {
            term_1 = (double)(term_1) * (double)(x) / (((Number)(i_1)).doubleValue());
            sum = (double)(sum) + (double)(term_1);
            i_1 = (long)((long)(i_1) + (long)(1));
        }
        return sum;
    }

    static double[] sigmoid(double[] vector) {
        double[] result = ((double[])(new double[]{}));
        long i_3 = 0L;
        while ((long)(i_3) < (long)(vector.length)) {
            double v_1 = (double)(vector[(int)((long)(i_3))]);
            double s_1 = 1.0 / (1.0 + (double)(exp_approx((double)(-v_1))));
            result = ((double[])(appendDouble(result, s_1)));
            i_3 = (long)((long)(i_3) + (long)(1));
        }
        return result;
    }

    static double[] swish(double[] vector, double beta) {
        double[] result_1 = ((double[])(new double[]{}));
        long i_5 = 0L;
        while ((long)(i_5) < (long)(vector.length)) {
            double v_3 = (double)(vector[(int)((long)(i_5))]);
            double s_3 = 1.0 / (1.0 + (double)(exp_approx((double)(-beta) * (double)(v_3))));
            result_1 = ((double[])(appendDouble(result_1, (double)(v_3) * s_3)));
            i_5 = (long)((long)(i_5) + (long)(1));
        }
        return result_1;
    }

    static double[] sigmoid_linear_unit(double[] vector) {
        return swish(((double[])(vector)), 1.0);
    }

    static boolean approx_equal(double a, double b, double eps) {
        double diff = (double)(a) > (double)(b) ? (double)(a) - (double)(b) : (double)(b) - (double)(a);
        return diff < (double)(eps);
    }

    static boolean approx_equal_list(double[] a, double[] b, double eps) {
        if ((long)(a.length) != (long)(b.length)) {
            return false;
        }
        long i_7 = 0L;
        while ((long)(i_7) < (long)(a.length)) {
            if (!(Boolean)approx_equal((double)(a[(int)((long)(i_7))]), (double)(b[(int)((long)(i_7))]), (double)(eps))) {
                return false;
            }
            i_7 = (long)((long)(i_7) + (long)(1));
        }
        return true;
    }

    static void test_swish() {
        double[] v_4 = ((double[])(new double[]{-1.0, 1.0, 2.0}));
        double eps_1 = 0.001;
        if (!(Boolean)approx_equal_list(((double[])(sigmoid(((double[])(v_4))))), ((double[])(new double[]{0.26894142, 0.73105858, 0.88079708})), eps_1)) {
            throw new RuntimeException(String.valueOf("sigmoid incorrect"));
        }
        if (!(Boolean)approx_equal_list(((double[])(sigmoid_linear_unit(((double[])(v_4))))), ((double[])(new double[]{-0.26894142, 0.73105858, 1.76159416})), eps_1)) {
            throw new RuntimeException(String.valueOf("sigmoid_linear_unit incorrect"));
        }
        if (!(Boolean)approx_equal_list(((double[])(swish(((double[])(v_4)), 2.0))), ((double[])(new double[]{-0.11920292, 0.88079708, 1.96402758})), eps_1)) {
            throw new RuntimeException(String.valueOf("swish incorrect"));
        }
        if (!(Boolean)approx_equal_list(((double[])(swish(((double[])(new double[]{-2.0})), 1.0))), ((double[])(new double[]{-0.23840584})), eps_1)) {
            throw new RuntimeException(String.valueOf("swish with parameter 1 incorrect"));
        }
    }

    static void main() {
        test_swish();
        System.out.println(_p(sigmoid(((double[])(new double[]{-1.0, 1.0, 2.0})))));
        System.out.println(_p(sigmoid_linear_unit(((double[])(new double[]{-1.0, 1.0, 2.0})))));
        System.out.println(_p(swish(((double[])(new double[]{-1.0, 1.0, 2.0})), 2.0)));
        System.out.println(_p(swish(((double[])(new double[]{-2.0})), 1.0)));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            main();
            long _benchDuration = _now() - _benchStart;
            long _benchMemory = _mem() - _benchMem;
            System.out.println("{");
            System.out.println("  \"duration_us\": " + _benchDuration + ",");
            System.out.println("  \"memory_bytes\": " + _benchMemory + ",");
            System.out.println("  \"name\": \"main\"");
            System.out.println("}");
            return;
        }
    }

    static boolean _nowSeeded = false;
    static int _nowSeed;
    static int _now() {
        if (!_nowSeeded) {
            String s = System.getenv("MOCHI_NOW_SEED");
            if (s != null && !s.isEmpty()) {
                try { _nowSeed = Integer.parseInt(s); _nowSeeded = true; } catch (Exception e) {}
            }
        }
        if (_nowSeeded) {
            _nowSeed = (int)((_nowSeed * 1664525L + 1013904223) % 2147483647);
            return _nowSeed;
        }
        return (int)(System.nanoTime() / 1000);
    }

    static long _mem() {
        Runtime rt = Runtime.getRuntime();
        rt.gc();
        return rt.totalMemory() - rt.freeMemory();
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
            if (d == Math.rint(d)) return String.valueOf((long) d);
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
