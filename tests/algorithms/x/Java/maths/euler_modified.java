public class Main {

    static int ceil_float(double x) {
        int i = ((Number)(x)).intValue();
        if (x > (((Number)(i)).doubleValue())) {
            return i + 1;
        }
        return i;
    }

    static double exp_approx(double x) {
        double term = 1.0;
        double sum = 1.0;
        int n = 1;
        while (n < 20) {
            term = term * x / (((Number)(n)).doubleValue());
            sum = sum + term;
            n = n + 1;
        }
        return sum;
    }

    static double[] euler_modified(java.util.function.BiFunction<Double,Double,Double> ode_func, double y0, double x0, double step, double x_end) {
        int n_1 = ceil_float((x_end - x0) / step);
        double[] y = ((double[])(new double[]{y0}));
        double x = x0;
        int k = 0;
        while (k < n_1) {
            double y_predict = y[k] + step * ode_func.apply(x, y[k]);
            double slope1 = ode_func.apply(x, y[k]);
            double slope2 = ode_func.apply(x + step, y_predict);
            double y_next = y[k] + (step / 2.0) * (slope1 + slope2);
            y = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(y), java.util.stream.DoubleStream.of(y_next)).toArray()));
            x = x + step;
            k = k + 1;
        }
        return y;
    }

    static double f1(double x, double y) {
        return -2.0 * x * y * y;
    }

    static double f2(double x, double y) {
        return -2.0 * y + (x * x * x) * exp_approx(-2.0 * x);
    }

    static void main() {
        double[] y1 = ((double[])(euler_modified(Main::f1, 1.0, 0.0, 0.2, 1.0)));
        System.out.println(y1[y1.length - 1]);
        double[] y2 = ((double[])(euler_modified(Main::f2, 1.0, 0.0, 0.1, 0.3)));
        System.out.println(y2[y2.length - 1]);
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
