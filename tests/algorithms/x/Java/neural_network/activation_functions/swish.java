public class Main {

    static double exp_approx(double x) {
        double sum = 1.0;
        double term = 1.0;
        int i = 1;
        while (i <= 20) {
            term = term * x / (((Number)(i)).doubleValue());
            sum = sum + term;
            i = i + 1;
        }
        return sum;
    }

    static double[] sigmoid(double[] vector) {
        double[] result = ((double[])(new double[]{}));
        int i_1 = 0;
        while (i_1 < vector.length) {
            double v = vector[i_1];
            double s = 1.0 / (1.0 + exp_approx(-v));
            result = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(result), java.util.stream.DoubleStream.of(s)).toArray()));
            i_1 = i_1 + 1;
        }
        return result;
    }

    static double[] swish(double[] vector, double beta) {
        double[] result_1 = ((double[])(new double[]{}));
        int i_2 = 0;
        while (i_2 < vector.length) {
            double v_1 = vector[i_2];
            double s_1 = 1.0 / (1.0 + exp_approx(-beta * v_1));
            result_1 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(result_1), java.util.stream.DoubleStream.of(v_1 * s_1)).toArray()));
            i_2 = i_2 + 1;
        }
        return result_1;
    }

    static double[] sigmoid_linear_unit(double[] vector) {
        return swish(((double[])(vector)), 1.0);
    }

    static boolean approx_equal(double a, double b, double eps) {
        double diff = a > b ? a - b : b - a;
        return diff < eps;
    }

    static boolean approx_equal_list(double[] a, double[] b, double eps) {
        if (a.length != b.length) {
            return false;
        }
        int i_3 = 0;
        while (i_3 < a.length) {
            if (!(Boolean)approx_equal(a[i_3], b[i_3], eps)) {
                return false;
            }
            i_3 = i_3 + 1;
        }
        return true;
    }

    static void test_swish() {
        double[] v_2 = ((double[])(new double[]{-1.0, 1.0, 2.0}));
        double eps = 0.001;
        if (!(Boolean)approx_equal_list(((double[])(sigmoid(((double[])(v_2))))), ((double[])(new double[]{0.26894142, 0.73105858, 0.88079708})), eps)) {
            throw new RuntimeException(String.valueOf("sigmoid incorrect"));
        }
        if (!(Boolean)approx_equal_list(((double[])(sigmoid_linear_unit(((double[])(v_2))))), ((double[])(new double[]{-0.26894142, 0.73105858, 1.76159416})), eps)) {
            throw new RuntimeException(String.valueOf("sigmoid_linear_unit incorrect"));
        }
        if (!(Boolean)approx_equal_list(((double[])(swish(((double[])(v_2)), 2.0))), ((double[])(new double[]{-0.11920292, 0.88079708, 1.96402758})), eps)) {
            throw new RuntimeException(String.valueOf("swish incorrect"));
        }
        if (!(Boolean)approx_equal_list(((double[])(swish(((double[])(new double[]{-2.0})), 1.0))), ((double[])(new double[]{-0.23840584})), eps)) {
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
        return String.valueOf(v);
    }
}
