public class Main {
    static long N_STEPS;

    static double floor(double x) {
        long i = ((Number)(x)).intValue();
        if ((((Number)(i)).doubleValue()) > x) {
            i = i - 1;
        }
        return ((Number)(i)).doubleValue();
    }

    static double pow10(long n) {
        double p = 1.0;
        long i_2 = 0;
        while (i_2 < n) {
            p = p * 10.0;
            i_2 = i_2 + 1;
        }
        return p;
    }

    static double round(double x, long n) {
        double m = pow10(n);
        return floor(x * m + 0.5) / m;
    }

    static double simpson_integration(java.util.function.Function<Double,Double> f, double a, double b, long precision) {
        if (precision <= 0) {
            throw new RuntimeException(String.valueOf("precision should be positive"));
        }
        double h_1 = (b - a) / (((Number)(N_STEPS)).doubleValue());
        double result_1 = f.apply(a) + f.apply(b);
        long i_4 = 1;
        while (i_4 < N_STEPS) {
            double x_1 = a + h_1 * (((Number)(i_4)).doubleValue());
            if (Math.floorMod(i_4, 2) == 1) {
                result_1 = result_1 + 4.0 * f.apply(x_1);
            } else {
                result_1 = result_1 + 2.0 * f.apply(x_1);
            }
            i_4 = i_4 + 1;
        }
        result_1 = result_1 * (h_1 / 3.0);
        double r_1 = round(result_1, precision);
        return r_1;
    }

    static double square(double x) {
        return x * x;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            N_STEPS = 1000;
            System.out.println(_p(simpson_integration(Main::square, 1.0, 2.0, 3)));
            System.out.println(_p(simpson_integration(Main::square, 3.45, 3.2, 1)));
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
