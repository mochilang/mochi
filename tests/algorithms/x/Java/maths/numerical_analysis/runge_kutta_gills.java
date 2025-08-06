public class Main {
    static double[] y1;
    static double[] y2;

    static double sqrt(double x) {
        double guess = x > 1.0 ? x / 2.0 : 1.0;
        int i = 0;
        while (i < 20) {
            guess = 0.5 * (guess + x / guess);
            i = i + 1;
        }
        return guess;
    }

    static double[] runge_kutta_gills(java.util.function.BiFunction<Double,Double,Double> func, double x_initial, double y_initial, double step_size, double x_final) {
        if (x_initial >= x_final) {
            throw new RuntimeException(String.valueOf("The final value of x must be greater than initial value of x."));
        }
        if (step_size <= 0.0) {
            throw new RuntimeException(String.valueOf("Step size must be positive."));
        }
        int n = ((Number)(((x_final - x_initial) / step_size))).intValue();
        double[] y = ((double[])(new double[]{}));
        int i_1 = 0;
        while (i_1 <= n) {
            y = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(y), java.util.stream.DoubleStream.of(0.0)).toArray()));
            i_1 = i_1 + 1;
        }
y[0] = y_initial;
        double xi = x_initial;
        int idx = 0;
        double root2 = sqrt(2.0);
        while (idx < n) {
            double k1 = step_size * func.apply(xi, y[idx]);
            double k2 = step_size * func.apply(xi + step_size / 2.0, y[idx] + k1 / 2.0);
            double k3 = step_size * func.apply(xi + step_size / 2.0, y[idx] + (-0.5 + 1.0 / root2) * k1 + (1.0 - 1.0 / root2) * k2);
            double k4 = step_size * func.apply(xi + step_size, y[idx] - (1.0 / root2) * k2 + (1.0 + 1.0 / root2) * k3);
y[idx + 1] = y[idx] + (k1 + (2.0 - root2) * k2 + (2.0 + root2) * k3 + k4) / 6.0;
            xi = xi + step_size;
            idx = idx + 1;
        }
        return y;
    }

    static double f1(double x, double y) {
        return (x - y) / 2.0;
    }

    static double f2(double x, double y) {
        return x;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            y1 = ((double[])(runge_kutta_gills(Main::f1, 0.0, 3.0, 0.2, 5.0)));
            System.out.println(_p(_geto(y1, y1.length - 1)));
            y2 = ((double[])(runge_kutta_gills(Main::f2, -1.0, 0.0, 0.2, 0.0)));
            System.out.println(_p(y2));
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
