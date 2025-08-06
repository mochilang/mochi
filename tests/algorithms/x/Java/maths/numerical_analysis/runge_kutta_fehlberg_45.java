public class Main {

    static double[] runge_kutta_fehlberg_45(java.util.function.BiFunction<Double,Double,Double> func, double x_initial, double y_initial, double step_size, double x_final) {
        if (x_initial >= x_final) {
            throw new RuntimeException(String.valueOf("The final value of x must be greater than initial value of x."));
        }
        if (step_size <= 0.0) {
            throw new RuntimeException(String.valueOf("Step size must be positive."));
        }
        int n = ((Number)(((x_final - x_initial) / step_size))).intValue();
        double[] ys = ((double[])(new double[]{}));
        double x = x_initial;
        double y = y_initial;
        ys = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(ys), java.util.stream.DoubleStream.of(y)).toArray()));
        int i = 0;
        while (i < n) {
            double k1 = step_size * func.apply(x, y);
            double k2 = step_size * func.apply(x + step_size / 4.0, y + k1 / 4.0);
            double k3 = step_size * func.apply(x + (3.0 / 8.0) * step_size, y + (3.0 / 32.0) * k1 + (9.0 / 32.0) * k2);
            double k4 = step_size * func.apply(x + (12.0 / 13.0) * step_size, y + (1932.0 / 2197.0) * k1 - (7200.0 / 2197.0) * k2 + (7296.0 / 2197.0) * k3);
            double k5 = step_size * func.apply(x + step_size, y + (439.0 / 216.0) * k1 - 8.0 * k2 + (3680.0 / 513.0) * k3 - (845.0 / 4104.0) * k4);
            double k6 = step_size * func.apply(x + step_size / 2.0, y - (8.0 / 27.0) * k1 + 2.0 * k2 - (3544.0 / 2565.0) * k3 + (1859.0 / 4104.0) * k4 - (11.0 / 40.0) * k5);
            y = y + (16.0 / 135.0) * k1 + (6656.0 / 12825.0) * k3 + (28561.0 / 56430.0) * k4 - (9.0 / 50.0) * k5 + (2.0 / 55.0) * k6;
            x = x + step_size;
            ys = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(ys), java.util.stream.DoubleStream.of(y)).toArray()));
            i = i + 1;
        }
        return ys;
    }

    static void main() {
        java.util.function.BiFunction<Double,Double,Double>[] f1 = new java.util.function.BiFunction[1];
        f1[0] = (x_1, y_1) -> 1.0 + y_1 * y_1;
        double[] y1 = ((double[])(runge_kutta_fehlberg_45(f1[0], 0.0, 0.0, 0.2, 1.0)));
        System.out.println(y1[1]);
        java.util.function.BiFunction<Double,Double,Double>[] f2 = new java.util.function.BiFunction[1];
        f2[0] = (x_2, y_2) -> x_2;
        double[] y2 = ((double[])(runge_kutta_fehlberg_45(f2[0], -1.0, 0.0, 0.2, 0.0)));
        System.out.println(y2[1]);
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
