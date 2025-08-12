public class Main {

    static double to_float(long x) {
        return (double)(x) * 1.0;
    }

    static double ln(double x) {
        if ((double)(x) <= 0.0) {
            throw new RuntimeException(String.valueOf("ln domain error"));
        }
        double y_1 = ((double)(x) - 1.0) / ((double)(x) + 1.0);
        double y2_1 = y_1 * y_1;
        double term_1 = y_1;
        double sum_1 = 0.0;
        long k_1 = 0L;
        while ((long)(k_1) < (long)(10)) {
            double denom_1 = ((Number)((long)((long)(2) * (long)(k_1)) + (long)(1))).doubleValue();
            sum_1 = sum_1 + term_1 / denom_1;
            term_1 = term_1 * y2_1;
            k_1 = (long)((long)(k_1) + (long)(1));
        }
        return 2.0 * sum_1;
    }

    static double exp(double x) {
        double term_2 = 1.0;
        double sum_3 = 1.0;
        long n_1 = 1L;
        while ((long)(n_1) < (long)(20)) {
            term_2 = term_2 * (double)(x) / ((Number)(n_1)).doubleValue();
            sum_3 = sum_3 + term_2;
            n_1 = (long)((long)(n_1) + (long)(1));
        }
        return sum_3;
    }

    static double pow_float(double base, double exponent) {
        return exp((double)(exponent) * (double)(ln((double)(base))));
    }

    static double get_altitude_at_pressure(double pressure) {
        if ((double)(pressure) > 101325.0) {
            throw new RuntimeException(String.valueOf("Value Higher than Pressure at Sea Level !"));
        }
        if ((double)(pressure) < 0.0) {
            throw new RuntimeException(String.valueOf("Atmospheric Pressure can not be negative !"));
        }
        double ratio_1 = (double)(pressure) / 101325.0;
        return 44330.0 * (1.0 - (double)(pow_float(ratio_1, 1.0 / 5.5255)));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(_p(get_altitude_at_pressure(100000.0)));
            System.out.println(_p(get_altitude_at_pressure(101325.0)));
            System.out.println(_p(get_altitude_at_pressure(80000.0)));
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
            if (d == Math.rint(d)) return String.valueOf((long) d);
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
