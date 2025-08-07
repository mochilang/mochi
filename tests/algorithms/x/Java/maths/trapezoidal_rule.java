public class Main {
    static double a_1;
    static double b_1;
    static double steps;
    static double[] boundary;
    static double y_1;

    static double f(double x) {
        return x * x;
    }

    static double[] make_points(double a, double b, double h) {
        double[] xs = ((double[])(new double[]{}));
        double x = a + h;
        while (x <= (b - h)) {
            xs = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(xs), java.util.stream.DoubleStream.of(x)).toArray()));
            x = x + h;
        }
        return xs;
    }

    static double trapezoidal_rule(double[] boundary, double steps) {
        double h = (boundary[1] - boundary[0]) / steps;
        double a = boundary[0];
        double b = boundary[1];
        double[] xs_1 = ((double[])(make_points(a, b, h)));
        double y = (h / 2.0) * f(a);
        int i = 0;
        while (i < xs_1.length) {
            y = y + h * f(xs_1[i]);
            i = i + 1;
        }
        y = y + (h / 2.0) * f(b);
        return y;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            a_1 = 0.0;
            b_1 = 1.0;
            steps = 10.0;
            boundary = ((double[])(new double[]{a_1, b_1}));
            y_1 = trapezoidal_rule(((double[])(boundary)), steps);
            System.out.println("y = " + _p(y_1));
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
