public class Main {

    static long ceil_float(double x) {
        long i = ((Number)(x)).intValue();
        if (x > (((Number)(i)).doubleValue())) {
            return i + 1;
        }
        return i;
    }

    static double exp_approx(double x) {
        double term = 1.0;
        double sum_1 = 1.0;
        long n_1 = 1;
        while (n_1 < 20) {
            term = term * x / (((Number)(n_1)).doubleValue());
            sum_1 = sum_1 + term;
            n_1 = n_1 + 1;
        }
        return sum_1;
    }

    static double[] euler_modified(java.util.function.BiFunction<Double,Double,Double> ode_func, double y0, double x0, double step, double x_end) {
        long n_2 = ceil_float((x_end - x0) / step);
        double[] y_1 = ((double[])(new double[]{y0}));
        double x_1 = x0;
        long k_1 = 0;
        while (k_1 < n_2) {
            double y_predict_1 = y_1[(int)(k_1)] + step * ode_func.apply(x_1, y_1[(int)(k_1)]);
            double slope1_1 = ode_func.apply(x_1, y_1[(int)(k_1)]);
            double slope2_1 = ode_func.apply(x_1 + step, y_predict_1);
            double y_next_1 = y_1[(int)(k_1)] + (step / 2.0) * (slope1_1 + slope2_1);
            y_1 = ((double[])(appendDouble(y_1, y_next_1)));
            x_1 = x_1 + step;
            k_1 = k_1 + 1;
        }
        return y_1;
    }

    static double f1(double x, double y) {
        return -2.0 * x * y * y;
    }

    static double f2(double x, double y) {
        return -2.0 * y + (x * x * x) * exp_approx(-2.0 * x);
    }

    static void main() {
        double[] y1 = ((double[])(euler_modified(Main::f1, 1.0, 0.0, 0.2, 1.0)));
        System.out.println(y1[(int)(y1.length - 1)]);
        double[] y2_1 = ((double[])(euler_modified(Main::f2, 1.0, 0.0, 0.1, 0.3)));
        System.out.println(y2_1[(int)(y2_1.length - 1)]);
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

    static double[] appendDouble(double[] arr, double v) {
        double[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }
}
