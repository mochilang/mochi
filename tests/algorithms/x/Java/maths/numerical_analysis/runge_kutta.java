public class Main {

    static double[] runge_kutta(java.util.function.BiFunction<Double,Double,Double> f, double y0, double x0, double h, double x_end) {
        double span = (x_end - x0) / h;
        int n = ((Number)(span)).intValue();
        if (((Number)(n)).doubleValue() < span) {
            n = n + 1;
        }
        double[] y = ((double[])(new double[]{}));
        int i = 0;
        while (i < n + 1) {
            y = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(y), java.util.stream.DoubleStream.of(0.0)).toArray()));
            i = i + 1;
        }
y[0] = y0;
        double x = x0;
        int k = 0;
        while (k < n) {
            double k1 = f[0].apply(x, y[k]);
            double k2 = f[0].apply(x + 0.5 * h, y[k] + 0.5 * h * k1);
            double k3 = f[0].apply(x + 0.5 * h, y[k] + 0.5 * h * k2);
            double k4 = f[0].apply(x + h, y[k] + h * k3);
y[k + 1] = y[k] + (1.0 / 6.0) * h * (k1 + 2.0 * k2 + 2.0 * k3 + k4);
            x = x + h;
            k = k + 1;
        }
        return y;
    }

    static void test_runge_kutta() {
        java.util.function.BiFunction<Double,Double,Double>[] f = new java.util.function.BiFunction[1];
        f[0] = (x_1, y_1) -> y_1;
        double[] result = ((double[])(runge_kutta(f[0], 1.0, 0.0, 0.01, 5.0)));
        double last = result[result.length - 1];
        double expected = 148.41315904125113;
        double diff = last - expected;
        if (diff < 0.0) {
            diff = -diff;
        }
        if (diff > 1e-06) {
            throw new RuntimeException(String.valueOf("runge_kutta failed"));
        }
    }

    static void main() {
        test_runge_kutta();
        java.util.function.BiFunction<Double,Double,Double>[] f = new java.util.function.BiFunction[1];
        f[0] = (x_2, y_2) -> y_2;
        double[] r = ((double[])(runge_kutta(f[0], 1.0, 0.0, 0.1, 1.0)));
        System.out.println(_p(_geto(r, r.length - 1)));
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

    static Object _geto(Object[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
