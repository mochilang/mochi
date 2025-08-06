public class Main {
    static class NRResult {
        double root;
        double error;
        double[] steps;
        NRResult(double root, double error, double[] steps) {
            this.root = root;
            this.error = error;
            this.steps = steps;
        }
        NRResult() {}
        @Override public String toString() {
            return String.format("{'root': %s, 'error': %s, 'steps': %s}", String.valueOf(root), String.valueOf(error), String.valueOf(steps));
        }
    }

    static NRResult result;

    static double abs_float(double x) {
        if (x < 0.0) {
            return -x;
        } else {
            return x;
        }
    }

    static void fail(String msg) {
        System.out.println("error: " + msg);
    }

    static double calc_derivative(java.util.function.Function<Double,Double> f, double x, double delta_x) {
        return (f.apply(x + delta_x / 2.0) - f.apply(x - delta_x / 2.0)) / delta_x;
    }

    static NRResult newton_raphson(java.util.function.Function<Double,Double> f, double x0, int max_iter, double step, double max_error, boolean log_steps) {
        double a = x0;
        double[] steps = ((double[])(new double[]{}));
        int i = 0;
        while (i < max_iter) {
            if (((Boolean)(log_steps))) {
                steps = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(steps), java.util.stream.DoubleStream.of(a)).toArray()));
            }
            double err = abs_float(f.apply(a));
            if (err < max_error) {
                return new NRResult(a, err, steps);
            }
            double der = calc_derivative(f, a, step);
            if (der == 0.0) {
                fail("No converging solution found, zero derivative");
                return new NRResult(a, err, steps);
            }
            a = a - f.apply(a) / der;
            i = i + 1;
        }
        fail("No converging solution found, iteration limit reached");
        return new NRResult(a, abs_float(f.apply(a)), steps);
    }

    static double poly(double x) {
        return x * x - 5.0 * x + 2.0;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            result = newton_raphson(Main::poly, 0.4, 20, 1e-06, 1e-06, false);
            System.out.println("root = " + _p(result.root) + ", error = " + _p(result.error));
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
