public class Main {
    static double r1;
    static double r2;
    static double r3;

    static double exp_approx(double x) {
        double y = (double)(x);
        boolean is_neg_1 = false;
        if ((double)(x) < (double)(0.0)) {
            is_neg_1 = true;
            y = (double)(-x);
        }
        double term_1 = (double)(1.0);
        double sum_1 = (double)(1.0);
        long n_1 = 1L;
        while ((long)(n_1) < 30L) {
            term_1 = (double)((double)((double)(term_1) * (double)(y)) / (double)((((Number)(n_1)).doubleValue())));
            sum_1 = (double)((double)(sum_1) + (double)(term_1));
            n_1 = (long)((long)(n_1) + 1L);
        }
        if (is_neg_1) {
            return (double)(1.0) / (double)(sum_1);
        }
        return sum_1;
    }

    static double ln_series(double x) {
        double t = (double)((double)(((double)(x) - (double)(1.0))) / (double)(((double)(x) + (double)(1.0))));
        double term_3 = (double)(t);
        double sum_3 = (double)(0.0);
        long n_3 = 1L;
        while ((long)(n_3) <= 19L) {
            sum_3 = (double)((double)(sum_3) + (double)((double)(term_3) / (double)((((Number)(n_3)).doubleValue()))));
            term_3 = (double)((double)((double)(term_3) * (double)(t)) * (double)(t));
            n_3 = (long)((long)(n_3) + 2L);
        }
        return (double)(2.0) * (double)(sum_3);
    }

    static double ln(double x) {
        double y_1 = (double)(x);
        long k_1 = 0L;
        while ((double)(y_1) >= (double)(10.0)) {
            y_1 = (double)((double)(y_1) / (double)(10.0));
            k_1 = (long)((long)(k_1) + 1L);
        }
        while ((double)(y_1) < (double)(1.0)) {
            y_1 = (double)((double)(y_1) * (double)(10.0));
            k_1 = (long)((long)(k_1) - 1L);
        }
        return (double)(ln_series((double)(y_1))) + (double)((double)((((Number)(k_1)).doubleValue())) * (double)(ln_series((double)(10.0))));
    }

    static double powf(double base, double exponent) {
        return exp_approx((double)((double)(exponent) * (double)(ln((double)(base)))));
    }

    static double rainfall_intensity(double coefficient_k, double coefficient_a, double coefficient_b, double coefficient_c, double return_period, double duration) {
        if ((double)(coefficient_k) <= (double)(0.0)) {
            throw new RuntimeException(String.valueOf("All parameters must be positive."));
        }
        if ((double)(coefficient_a) <= (double)(0.0)) {
            throw new RuntimeException(String.valueOf("All parameters must be positive."));
        }
        if ((double)(coefficient_b) <= (double)(0.0)) {
            throw new RuntimeException(String.valueOf("All parameters must be positive."));
        }
        if ((double)(coefficient_c) <= (double)(0.0)) {
            throw new RuntimeException(String.valueOf("All parameters must be positive."));
        }
        if ((double)(return_period) <= (double)(0.0)) {
            throw new RuntimeException(String.valueOf("All parameters must be positive."));
        }
        if ((double)(duration) <= (double)(0.0)) {
            throw new RuntimeException(String.valueOf("All parameters must be positive."));
        }
        double numerator_1 = (double)((double)(coefficient_k) * (double)(powf((double)(return_period), (double)(coefficient_a))));
        double denominator_1 = (double)(powf((double)((double)(duration) + (double)(coefficient_b)), (double)(coefficient_c)));
        return (double)(numerator_1) / (double)(denominator_1);
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            r1 = (double)(rainfall_intensity((double)(1000.0), (double)(0.2), (double)(11.6), (double)(0.81), (double)(10.0), (double)(60.0)));
            System.out.println(_p(r1));
            r2 = (double)(rainfall_intensity((double)(1000.0), (double)(0.2), (double)(11.6), (double)(0.81), (double)(10.0), (double)(30.0)));
            System.out.println(_p(r2));
            r3 = (double)(rainfall_intensity((double)(1000.0), (double)(0.2), (double)(11.6), (double)(0.81), (double)(5.0), (double)(60.0)));
            System.out.println(_p(r3));
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
        if (v instanceof Double || v instanceof Float) {
            double d = ((Number) v).doubleValue();
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
