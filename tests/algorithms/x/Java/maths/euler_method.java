public class Main {

    static long ceil_int(double x) {
        long n = ((Number)(x)).intValue();
        if (((Number)(n)).doubleValue() < x) {
            n = n + 1;
        }
        return n;
    }

    static double[] explicit_euler(java.util.function.BiFunction<Double,Double,Double> ode_func, double y0, double x0, double step_size, double x_end) {
        long n_1 = ceil_int((x_end - x0) / step_size);
        double[] y_1 = ((double[])(new double[]{}));
        long i_1 = 0;
        while (i_1 <= n_1) {
            y_1 = ((double[])(appendDouble(y_1, 0.0)));
            i_1 = i_1 + 1;
        }
y_1[(int)(0)] = y0;
        double x_1 = x0;
        long k_1 = 0;
        while (k_1 < n_1) {
y_1[(int)(k_1 + 1)] = y_1[(int)(k_1)] + step_size * ode_func.apply(x_1, y_1[(int)(k_1)]);
            x_1 = x_1 + step_size;
            k_1 = k_1 + 1;
        }
        return y_1;
    }

    static double abs_float(double a) {
        if (a < 0.0) {
            return -a;
        }
        return a;
    }

    static void test_explicit_euler() {
        java.util.function.BiFunction<Double,Double,Double> f = (x_2, y_2) -> y_2;
        double[] ys_1 = ((double[])(explicit_euler(f, 1.0, 0.0, 0.01, 5.0)));
        double last_1 = ys_1[(int)(ys_1.length - 1)];
        if (abs_float(last_1 - 144.77277243257308) > 0.001) {
            throw new RuntimeException(String.valueOf("explicit_euler failed"));
        }
    }

    static void main() {
        test_explicit_euler();
        java.util.function.BiFunction<Double,Double,Double> f_2 = (x_4, y_4) -> y_4;
        double[] ys_3 = ((double[])(explicit_euler(f_2, 1.0, 0.0, 0.01, 5.0)));
        System.out.println(ys_3[(int)(ys_3.length - 1)]);
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
