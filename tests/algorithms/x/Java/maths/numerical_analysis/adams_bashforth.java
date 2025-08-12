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
        if (x_initials[(int)(x_initials.length - 1)] >= x_final) {
            throw new RuntimeException(String.valueOf("The final value of x must be greater than the initial values of x."));
        }
        if (step_size <= 0.0) {
            throw new RuntimeException(String.valueOf("Step size must be positive."));
        }
        long i_1 = 0;
        while (i_1 < x_initials.length - 1) {
            double diff_1 = x_initials[(int)(i_1 + 1)] - x_initials[(int)(i_1)];
            if (abs_float(diff_1 - step_size) > 1e-10) {
                throw new RuntimeException(String.valueOf("x-values must be equally spaced according to step size."));
            }
            i_1 = i_1 + 1;
        }
    }

    static String list_to_string(double[] xs) {
        String s = "[";
        long i_3 = 0;
        while (i_3 < xs.length) {
            s = s + _p(_getd(xs, ((Number)(i_3)).intValue()));
            if (i_3 + 1 < xs.length) {
                s = s + ", ";
            }
            i_3 = i_3 + 1;
        }
        s = s + "]";
        return s;
    }

    static double[] adams_bashforth_step2(java.util.function.BiFunction<Double,Double,Double> f, double[] x_initials, double[] y_initials, double step_size, double x_final) {
        validate_inputs(((double[])(x_initials)), step_size, x_final);
        if (x_initials.length != 2 || y_initials.length != 2) {
            throw new RuntimeException(String.valueOf("Insufficient initial points information."));
        }
        double x0_1 = x_initials[(int)(0)];
        double x1_1 = x_initials[(int)(1)];
        double[] y_1 = ((double[])(new double[]{}));
        y_1 = ((double[])(appendDouble(y_1, y_initials[(int)(0)])));
        y_1 = ((double[])(appendDouble(y_1, y_initials[(int)(1)])));
        long n_1 = ((Number)(((x_final - x1_1) / step_size))).intValue();
        long i_5 = 0;
        while (i_5 < n_1) {
            double term_1 = 3.0 * f.apply(x1_1, y_1[(int)(i_5 + 1)]) - f.apply(x0_1, y_1[(int)(i_5)]);
            double y_next_1 = y_1[(int)(i_5 + 1)] + (step_size / 2.0) * term_1;
            y_1 = ((double[])(appendDouble(y_1, y_next_1)));
            x0_1 = x1_1;
            x1_1 = x1_1 + step_size;
            i_5 = i_5 + 1;
        }
        return y_1;
    }

    static double[] adams_bashforth_step3(java.util.function.BiFunction<Double,Double,Double> f, double[] x_initials, double[] y_initials, double step_size, double x_final) {
        validate_inputs(((double[])(x_initials)), step_size, x_final);
        if (x_initials.length != 3 || y_initials.length != 3) {
            throw new RuntimeException(String.valueOf("Insufficient initial points information."));
        }
        double x0_3 = x_initials[(int)(0)];
        double x1_3 = x_initials[(int)(1)];
        double x2_1 = x_initials[(int)(2)];
        double[] y_3 = ((double[])(new double[]{}));
        y_3 = ((double[])(appendDouble(y_3, y_initials[(int)(0)])));
        y_3 = ((double[])(appendDouble(y_3, y_initials[(int)(1)])));
        y_3 = ((double[])(appendDouble(y_3, y_initials[(int)(2)])));
        long n_3 = ((Number)(((x_final - x2_1) / step_size))).intValue();
        long i_7 = 0;
        while (i_7 <= n_3) {
            double term_3 = 23.0 * f.apply(x2_1, y_3[(int)(i_7 + 2)]) - 16.0 * f.apply(x1_3, y_3[(int)(i_7 + 1)]) + 5.0 * f.apply(x0_3, y_3[(int)(i_7)]);
            double y_next_3 = y_3[(int)(i_7 + 2)] + (step_size / 12.0) * term_3;
            y_3 = ((double[])(appendDouble(y_3, y_next_3)));
            x0_3 = x1_3;
            x1_3 = x2_1;
            x2_1 = x2_1 + step_size;
            i_7 = i_7 + 1;
        }
        return y_3;
    }

    static double[] adams_bashforth_step4(java.util.function.BiFunction<Double,Double,Double> f, double[] x_initials, double[] y_initials, double step_size, double x_final) {
        validate_inputs(((double[])(x_initials)), step_size, x_final);
        if (x_initials.length != 4 || y_initials.length != 4) {
            throw new RuntimeException(String.valueOf("Insufficient initial points information."));
        }
        double x0_5 = x_initials[(int)(0)];
        double x1_5 = x_initials[(int)(1)];
        double x2_3 = x_initials[(int)(2)];
        double x3_1 = x_initials[(int)(3)];
        double[] y_5 = ((double[])(new double[]{}));
        y_5 = ((double[])(appendDouble(y_5, y_initials[(int)(0)])));
        y_5 = ((double[])(appendDouble(y_5, y_initials[(int)(1)])));
        y_5 = ((double[])(appendDouble(y_5, y_initials[(int)(2)])));
        y_5 = ((double[])(appendDouble(y_5, y_initials[(int)(3)])));
        long n_5 = ((Number)(((x_final - x3_1) / step_size))).intValue();
        long i_9 = 0;
        while (i_9 < n_5) {
            double term_5 = 55.0 * f.apply(x3_1, y_5[(int)(i_9 + 3)]) - 59.0 * f.apply(x2_3, y_5[(int)(i_9 + 2)]) + 37.0 * f.apply(x1_5, y_5[(int)(i_9 + 1)]) - 9.0 * f.apply(x0_5, y_5[(int)(i_9)]);
            double y_next_5 = y_5[(int)(i_9 + 3)] + (step_size / 24.0) * term_5;
            y_5 = ((double[])(appendDouble(y_5, y_next_5)));
            x0_5 = x1_5;
            x1_5 = x2_3;
            x2_3 = x3_1;
            x3_1 = x3_1 + step_size;
            i_9 = i_9 + 1;
        }
        return y_5;
    }

    static double[] adams_bashforth_step5(java.util.function.BiFunction<Double,Double,Double> f, double[] x_initials, double[] y_initials, double step_size, double x_final) {
        validate_inputs(((double[])(x_initials)), step_size, x_final);
        if (x_initials.length != 5 || y_initials.length != 5) {
            throw new RuntimeException(String.valueOf("Insufficient initial points information."));
        }
        double x0_7 = x_initials[(int)(0)];
        double x1_7 = x_initials[(int)(1)];
        double x2_5 = x_initials[(int)(2)];
        double x3_3 = x_initials[(int)(3)];
        double x4_1 = x_initials[(int)(4)];
        double[] y_7 = ((double[])(new double[]{}));
        y_7 = ((double[])(appendDouble(y_7, y_initials[(int)(0)])));
        y_7 = ((double[])(appendDouble(y_7, y_initials[(int)(1)])));
        y_7 = ((double[])(appendDouble(y_7, y_initials[(int)(2)])));
        y_7 = ((double[])(appendDouble(y_7, y_initials[(int)(3)])));
        y_7 = ((double[])(appendDouble(y_7, y_initials[(int)(4)])));
        long n_7 = ((Number)(((x_final - x4_1) / step_size))).intValue();
        long i_11 = 0;
        while (i_11 <= n_7) {
            double term_7 = 1901.0 * f.apply(x4_1, y_7[(int)(i_11 + 4)]) - 2774.0 * f.apply(x3_3, y_7[(int)(i_11 + 3)]) - 2616.0 * f.apply(x2_5, y_7[(int)(i_11 + 2)]) - 1274.0 * f.apply(x1_7, y_7[(int)(i_11 + 1)]) + 251.0 * f.apply(x0_7, y_7[(int)(i_11)]);
            double y_next_7 = y_7[(int)(i_11 + 4)] + (step_size / 720.0) * term_7;
            y_7 = ((double[])(appendDouble(y_7, y_next_7)));
            x0_7 = x1_7;
            x1_7 = x2_5;
            x2_5 = x3_3;
            x3_3 = x4_1;
            x4_1 = x4_1 + step_size;
            i_11 = i_11 + 1;
        }
        return y_7;
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
            System.out.println(_p(_getd(y3, ((Number)(3)).intValue())));
            y4 = ((double[])(adams_bashforth_step4(Main::f_xy, ((double[])(new double[]{0.0, 0.2, 0.4, 0.6})), ((double[])(new double[]{0.0, 0.0, 0.04, 0.128})), 0.2, 1.0)));
            System.out.println(_p(_getd(y4, ((Number)(4)).intValue())));
            System.out.println(_p(_getd(y4, ((Number)(5)).intValue())));
            y5 = ((double[])(adams_bashforth_step5(Main::f_xy, ((double[])(new double[]{0.0, 0.2, 0.4, 0.6, 0.8})), ((double[])(new double[]{0.0, 0.0214, 0.0214, 0.22211, 0.42536})), 0.2, 1.0)));
            System.out.println(_p(_getd(y5, ((Number)(y5.length - 1)).intValue())));
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
            if (d == Math.rint(d)) return String.valueOf((long) d);
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }

    static Double _getd(double[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
