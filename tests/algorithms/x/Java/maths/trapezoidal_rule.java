public class Main {
    static double a_2 = (double)(0.0);
    static double b_2 = (double)(1.0);
    static double steps = (double)(10.0);
    static double[] boundary = ((double[])(new double[]{a_2, b_2}));
    static double y_2;

    static double f(double x) {
        return (double)(x) * (double)(x);
    }

    static double[] make_points(double a, double b, double h) {
        double[] xs = ((double[])(new double[]{}));
        double x_1 = (double)((double)(a) + (double)(h));
        while ((double)(x_1) <= (double)(((double)(b) - (double)(h)))) {
            xs = ((double[])(appendDouble(xs, (double)(x_1))));
            x_1 = (double)((double)(x_1) + (double)(h));
        }
        return xs;
    }

    static double trapezoidal_rule(double[] boundary, double steps) {
        double h = (double)((double)(((double)(boundary[(int)(1L)]) - (double)(boundary[(int)(0L)]))) / (double)(steps));
        double a_1 = (double)(boundary[(int)(0L)]);
        double b_1 = (double)(boundary[(int)(1L)]);
        double[] xs_2 = ((double[])(make_points((double)(a_1), (double)(b_1), (double)(h))));
        double y_1 = (double)((double)(((double)(h) / (double)(2.0))) * (double)(f((double)(a_1))));
        long i_1 = 0L;
        while ((long)(i_1) < (long)(xs_2.length)) {
            y_1 = (double)((double)(y_1) + (double)((double)(h) * (double)(f((double)(xs_2[(int)((long)(i_1))])))));
            i_1 = (long)((long)(i_1) + 1L);
        }
        y_1 = (double)((double)(y_1) + (double)((double)(((double)(h) / (double)(2.0))) * (double)(f((double)(b_1)))));
        return y_1;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            y_2 = (double)(trapezoidal_rule(((double[])(boundary)), (double)(steps)));
            System.out.println("y = " + _p(y_2));
            long _benchDuration = _now() - _benchStart;
            long _benchMemory = _mem() - _benchMem;
            System.out.println("{\"duration_us\": " + _benchDuration + ", \"memory_bytes\": " + _benchMemory + ", \"name\": \"main\"}");
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
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
