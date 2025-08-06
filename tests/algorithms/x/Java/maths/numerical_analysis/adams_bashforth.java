public class Main {
    static double[] y2;
    static double[] y3;
    static double[] y4;
    static double[] y5;

    static double abs_float(double x) {
        if (x < 0.0) {
            return -x;
        } else {
            return x;
        }
    }

    static void validate_inputs(double[] x_initials, double step_size, double x_final) {
        if (x_initials[x_initials.length - 1] >= x_final) {
            throw new RuntimeException(String.valueOf("The final value of x must be greater than the initial values of x."));
        }
        if (step_size <= 0.0) {
            throw new RuntimeException(String.valueOf("Step size must be positive."));
        }
        int i = 0;
        while (i < x_initials.length - 1) {
            double diff = x_initials[i + 1] - x_initials[i];
            if (abs_float(diff - step_size) > 1e-10) {
                throw new RuntimeException(String.valueOf("x-values must be equally spaced according to step size."));
            }
            i = i + 1;
        }
    }

    static String list_to_string(double[] xs) {
        String s = "[";
        int i_1 = 0;
        while (i_1 < xs.length) {
            s = s + _p(_geto(xs, i_1));
            if (i_1 + 1 < xs.length) {
                s = s + ", ";
            }
            i_1 = i_1 + 1;
        }
        s = s + "]";
        return s;
    }

    static double[] adams_bashforth_step2(java.util.function.BiFunction<Double,Double,Double> f, double[] x_initials, double[] y_initials, double step_size, double x_final) {
        validate_inputs(((double[])(x_initials)), step_size, x_final);
        if (x_initials.length != 2 || y_initials.length != 2) {
            throw new RuntimeException(String.valueOf("Insufficient initial points information."));
        }
        double x0 = x_initials[0];
        double x1 = x_initials[1];
        double[] y = ((double[])(new double[]{}));
        y = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(y), java.util.stream.DoubleStream.of(y_initials[0])).toArray()));
        y = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(y), java.util.stream.DoubleStream.of(y_initials[1])).toArray()));
        int n = ((Number)(((x_final - x1) / step_size))).intValue();
        int i_2 = 0;
        while (i_2 < n) {
            double term = 3.0 * f.apply(x1, y[i_2 + 1]) - f.apply(x0, y[i_2]);
            double y_next = y[i_2 + 1] + (step_size / 2.0) * term;
            y = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(y), java.util.stream.DoubleStream.of(y_next)).toArray()));
            x0 = x1;
            x1 = x1 + step_size;
            i_2 = i_2 + 1;
        }
        return y;
    }

    static double[] adams_bashforth_step3(java.util.function.BiFunction<Double,Double,Double> f, double[] x_initials, double[] y_initials, double step_size, double x_final) {
        validate_inputs(((double[])(x_initials)), step_size, x_final);
        if (x_initials.length != 3 || y_initials.length != 3) {
            throw new RuntimeException(String.valueOf("Insufficient initial points information."));
        }
        double x0_1 = x_initials[0];
        double x1_1 = x_initials[1];
        double x2 = x_initials[2];
        double[] y_1 = ((double[])(new double[]{}));
        y_1 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(y_1), java.util.stream.DoubleStream.of(y_initials[0])).toArray()));
        y_1 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(y_1), java.util.stream.DoubleStream.of(y_initials[1])).toArray()));
        y_1 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(y_1), java.util.stream.DoubleStream.of(y_initials[2])).toArray()));
        int n_1 = ((Number)(((x_final - x2) / step_size))).intValue();
        int i_3 = 0;
        while (i_3 <= n_1) {
            double term_1 = 23.0 * f.apply(x2, y_1[i_3 + 2]) - 16.0 * f.apply(x1_1, y_1[i_3 + 1]) + 5.0 * f.apply(x0_1, y_1[i_3]);
            double y_next_1 = y_1[i_3 + 2] + (step_size / 12.0) * term_1;
            y_1 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(y_1), java.util.stream.DoubleStream.of(y_next_1)).toArray()));
            x0_1 = x1_1;
            x1_1 = x2;
            x2 = x2 + step_size;
            i_3 = i_3 + 1;
        }
        return y_1;
    }

    static double[] adams_bashforth_step4(java.util.function.BiFunction<Double,Double,Double> f, double[] x_initials, double[] y_initials, double step_size, double x_final) {
        validate_inputs(((double[])(x_initials)), step_size, x_final);
        if (x_initials.length != 4 || y_initials.length != 4) {
            throw new RuntimeException(String.valueOf("Insufficient initial points information."));
        }
        double x0_2 = x_initials[0];
        double x1_2 = x_initials[1];
        double x2_1 = x_initials[2];
        double x3 = x_initials[3];
        double[] y_2 = ((double[])(new double[]{}));
        y_2 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(y_2), java.util.stream.DoubleStream.of(y_initials[0])).toArray()));
        y_2 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(y_2), java.util.stream.DoubleStream.of(y_initials[1])).toArray()));
        y_2 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(y_2), java.util.stream.DoubleStream.of(y_initials[2])).toArray()));
        y_2 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(y_2), java.util.stream.DoubleStream.of(y_initials[3])).toArray()));
        int n_2 = ((Number)(((x_final - x3) / step_size))).intValue();
        int i_4 = 0;
        while (i_4 < n_2) {
            double term_2 = 55.0 * f.apply(x3, y_2[i_4 + 3]) - 59.0 * f.apply(x2_1, y_2[i_4 + 2]) + 37.0 * f.apply(x1_2, y_2[i_4 + 1]) - 9.0 * f.apply(x0_2, y_2[i_4]);
            double y_next_2 = y_2[i_4 + 3] + (step_size / 24.0) * term_2;
            y_2 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(y_2), java.util.stream.DoubleStream.of(y_next_2)).toArray()));
            x0_2 = x1_2;
            x1_2 = x2_1;
            x2_1 = x3;
            x3 = x3 + step_size;
            i_4 = i_4 + 1;
        }
        return y_2;
    }

    static double[] adams_bashforth_step5(java.util.function.BiFunction<Double,Double,Double> f, double[] x_initials, double[] y_initials, double step_size, double x_final) {
        validate_inputs(((double[])(x_initials)), step_size, x_final);
        if (x_initials.length != 5 || y_initials.length != 5) {
            throw new RuntimeException(String.valueOf("Insufficient initial points information."));
        }
        double x0_3 = x_initials[0];
        double x1_3 = x_initials[1];
        double x2_2 = x_initials[2];
        double x3_1 = x_initials[3];
        double x4 = x_initials[4];
        double[] y_3 = ((double[])(new double[]{}));
        y_3 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(y_3), java.util.stream.DoubleStream.of(y_initials[0])).toArray()));
        y_3 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(y_3), java.util.stream.DoubleStream.of(y_initials[1])).toArray()));
        y_3 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(y_3), java.util.stream.DoubleStream.of(y_initials[2])).toArray()));
        y_3 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(y_3), java.util.stream.DoubleStream.of(y_initials[3])).toArray()));
        y_3 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(y_3), java.util.stream.DoubleStream.of(y_initials[4])).toArray()));
        int n_3 = ((Number)(((x_final - x4) / step_size))).intValue();
        int i_5 = 0;
        while (i_5 <= n_3) {
            double term_3 = 1901.0 * f.apply(x4, y_3[i_5 + 4]) - 2774.0 * f.apply(x3_1, y_3[i_5 + 3]) - 2616.0 * f.apply(x2_2, y_3[i_5 + 2]) - 1274.0 * f.apply(x1_3, y_3[i_5 + 1]) + 251.0 * f.apply(x0_3, y_3[i_5]);
            double y_next_3 = y_3[i_5 + 4] + (step_size / 720.0) * term_3;
            y_3 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(y_3), java.util.stream.DoubleStream.of(y_next_3)).toArray()));
            x0_3 = x1_3;
            x1_3 = x2_2;
            x2_2 = x3_1;
            x3_1 = x4;
            x4 = x4 + step_size;
            i_5 = i_5 + 1;
        }
        return y_3;
    }

    static double f_x(double x, double y) {
        return x;
    }

    static double f_xy(double x, double y) {
        return x + y;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            y2 = ((double[])(adams_bashforth_step2(Main::f_x, ((double[])(new double[]{0.0, 0.2})), ((double[])(new double[]{0.0, 0.0})), 0.2, 1.0)));
            System.out.println(list_to_string(((double[])(y2))));
            y3 = ((double[])(adams_bashforth_step3(Main::f_xy, ((double[])(new double[]{0.0, 0.2, 0.4})), ((double[])(new double[]{0.0, 0.0, 0.04})), 0.2, 1.0)));
            System.out.println(_p(_geto(y3, 3)));
            y4 = ((double[])(adams_bashforth_step4(Main::f_xy, ((double[])(new double[]{0.0, 0.2, 0.4, 0.6})), ((double[])(new double[]{0.0, 0.0, 0.04, 0.128})), 0.2, 1.0)));
            System.out.println(_p(_geto(y4, 4)));
            System.out.println(_p(_geto(y4, 5)));
            y5 = ((double[])(adams_bashforth_step5(Main::f_xy, ((double[])(new double[]{0.0, 0.2, 0.4, 0.6, 0.8})), ((double[])(new double[]{0.0, 0.0214, 0.0214, 0.22211, 0.42536})), 0.2, 1.0)));
            System.out.println(_p(_geto(y5, y5.length - 1)));
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
