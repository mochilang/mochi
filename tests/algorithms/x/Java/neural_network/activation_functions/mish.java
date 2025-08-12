public class Main {

    static double exp_approx(double x) {
        boolean neg = false;
        double y_1 = (double)(x);
        if ((double)(x) < 0.0) {
            neg = true;
            y_1 = (double)(-x);
        }
        double term_1 = 1.0;
        double sum_1 = 1.0;
        long n_1 = 1L;
        while ((long)(n_1) < (long)(30)) {
            term_1 = term_1 * y_1 / (((Number)(n_1)).doubleValue());
            sum_1 = sum_1 + term_1;
            n_1 = (long)((long)(n_1) + (long)(1));
        }
        if (neg) {
            return 1.0 / sum_1;
        }
        return sum_1;
    }

    static double ln_series(double x) {
        double t = ((double)(x) - 1.0) / ((double)(x) + 1.0);
        double term_3 = t;
        double acc_1 = 0.0;
        long n_3 = 1L;
        while ((long)(n_3) <= (long)(19)) {
            acc_1 = acc_1 + term_3 / (((Number)(n_3)).doubleValue());
            term_3 = term_3 * t * t;
            n_3 = (long)((long)(n_3) + (long)(2));
        }
        return 2.0 * acc_1;
    }

    static double ln(double x) {
        double y_2 = (double)(x);
        long k_1 = 0L;
        while (y_2 >= 10.0) {
            y_2 = y_2 / 10.0;
            k_1 = (long)((long)(k_1) + (long)(1));
        }
        while (y_2 < 1.0) {
            y_2 = y_2 * 10.0;
            k_1 = (long)((long)(k_1) - (long)(1));
        }
        return (double)(ln_series(y_2)) + (((Number)(k_1)).doubleValue()) * (double)(ln_series(10.0));
    }

    static double softplus(double x) {
        return ln(1.0 + (double)(exp_approx((double)(x))));
    }

    static double tanh_approx(double x) {
        return (2.0 / (1.0 + (double)(exp_approx(-2.0 * (double)(x))))) - 1.0;
    }

    static double[] mish(double[] vector) {
        double[] result = ((double[])(new double[]{}));
        long i_1 = 0L;
        while ((long)(i_1) < (long)(vector.length)) {
            double x_1 = (double)(vector[(int)((long)(i_1))]);
            double sp_1 = (double)(softplus((double)(x_1)));
            double y_4 = (double)(x_1) * (double)(tanh_approx((double)(sp_1)));
            result = ((double[])(appendDouble(result, y_4)));
            i_1 = (long)((long)(i_1) + (long)(1));
        }
        return result;
    }

    static void main() {
        double[] v1 = ((double[])(new double[]{2.3, 0.6, -2.0, -3.8}));
        double[] v2_1 = ((double[])(new double[]{-9.2, -0.3, 0.45, -4.56}));
        System.out.println(_p(mish(((double[])(v1)))));
        System.out.println(_p(mish(((double[])(v2_1)))));
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
