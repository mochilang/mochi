public class Main {

    static int ceil_int(double x) {
        int n = ((Number)(x)).intValue();
        if (((Number)(n)).doubleValue() < x) {
            n = n + 1;
        }
        return n;
    }

    static double[] explicit_euler(java.util.function.BiFunction<Double,Double,Double> ode_func, double y0, double x0, double step_size, double x_end) {
        int n_1 = ceil_int((x_end - x0) / step_size);
        double[] y = ((double[])(new double[]{}));
        int i = 0;
        while (i <= n_1) {
            y = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(y), java.util.stream.DoubleStream.of(0.0)).toArray()));
            i = i + 1;
        }
y[0] = y0;
        double x = x0;
        int k = 0;
        while (k < n_1) {
y[k + 1] = y[k] + step_size * ode_func.apply(x, y[k]);
            x = x + step_size;
            k = k + 1;
        }
        return y;
    }

    static double abs_float(double a) {
        if (a < 0.0) {
            return -a;
        }
        return a;
    }

    static void test_explicit_euler() {
        java.util.function.BiFunction<Double,Double,Double> f = (x_1, y_1) -> y_1;
        double[] ys = ((double[])(explicit_euler(f, 1.0, 0.0, 0.01, 5.0)));
        double last = ys[ys.length - 1];
        if (abs_float(last - 144.77277243257308) > 0.001) {
            throw new RuntimeException(String.valueOf("explicit_euler failed"));
        }
    }

    static void main() {
        test_explicit_euler();
        java.util.function.BiFunction<Double,Double,Double> f_1 = (x_2, y_2) -> y_2;
        double[] ys_1 = ((double[])(explicit_euler(f_1, 1.0, 0.0, 0.01, 5.0)));
        System.out.println(ys_1[ys_1.length - 1]);
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
}
