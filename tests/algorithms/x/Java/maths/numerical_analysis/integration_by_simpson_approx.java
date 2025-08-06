public class Main {
    static int N_STEPS;

    static double floor(double x) {
        int i = ((Number)(x)).intValue();
        if ((((Number)(i)).doubleValue()) > x) {
            i = i - 1;
        }
        return ((Number)(i)).doubleValue();
    }

    static double pow10(int n) {
        double p = 1.0;
        int i_1 = 0;
        while (i_1 < n) {
            p = p * 10.0;
            i_1 = i_1 + 1;
        }
        return p;
    }

    static double round(double x, int n) {
        double m = pow10(n);
        return floor(x * m + 0.5) / m;
    }

    static double simpson_integration(java.util.function.Function<Double,Double> f, double a, double b, int precision) {
        if (precision <= 0) {
            throw new RuntimeException(String.valueOf("precision should be positive"));
        }
        double h = (b - a) / (((Number)(N_STEPS)).doubleValue());
        double result = f.apply(a) + f.apply(b);
        int i_2 = 1;
        while (i_2 < N_STEPS) {
            double x = a + h * (((Number)(i_2)).doubleValue());
            if (Math.floorMod(i_2, 2) == 1) {
                result = result + 4.0 * f.apply(x);
            } else {
                result = result + 2.0 * f.apply(x);
            }
            i_2 = i_2 + 1;
        }
        result = result * (h / 3.0);
        double r = round(result, precision);
        return r;
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
        return String.valueOf(v);
    }
}
