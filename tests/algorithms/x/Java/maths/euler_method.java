public class Main {

    static long ceil_int(double x) {
        long n = (long)(((Number)(x)).intValue());
        if ((double)(((Number)(n)).doubleValue()) < (double)(x)) {
            n = (long)((long)(n) + 1L);
        }
        return n;
    }

    static double[] explicit_euler(java.util.function.BiFunction<Double,Double,Double> ode_func, double y0, double x0, double step_size, double x_end) {
        long n_1 = (long)(ceil_int((double)((double)(((double)(x_end) - (double)(x0))) / (double)(step_size))));
        double[] y_1 = ((double[])(new double[]{}));
        long i_1 = 0L;
        while ((long)(i_1) <= (long)(n_1)) {
            y_1 = ((double[])(appendDouble(y_1, (double)(0.0))));
            i_1 = (long)((long)(i_1) + 1L);
        }
y_1[(int)(0L)] = (double)(y0);
        double x_1 = (double)(x0);
        long k_1 = 0L;
        while ((long)(k_1) < (long)(n_1)) {
y_1[(int)((long)((long)(k_1) + 1L))] = (double)((double)(y_1[(int)((long)(k_1))]) + (double)((double)(step_size) * (double)(ode_func.apply((double)(x_1), (double)(y_1[(int)((long)(k_1))])))));
            x_1 = (double)((double)(x_1) + (double)(step_size));
            k_1 = (long)((long)(k_1) + 1L);
        }
        return y_1;
    }

    static double abs_float(double a) {
        if ((double)(a) < (double)(0.0)) {
            return -a;
        }
        return a;
    }

    static void test_explicit_euler() {
        java.util.function.BiFunction<Double,Double,Double> f = (x, y) -> y;
        double[] ys_1 = ((double[])(explicit_euler(f, (double)(1.0), (double)(0.0), (double)(0.01), (double)(5.0))));
        double last_1 = (double)(ys_1[(int)((long)((long)(ys_1.length) - 1L))]);
        if ((double)(abs_float((double)((double)(last_1) - (double)(144.77277243257308)))) > (double)(0.001)) {
            throw new RuntimeException(String.valueOf("explicit_euler failed"));
        }
    }

    static void main() {
        test_explicit_euler();
        java.util.function.BiFunction<Double,Double,Double> f_2 = (x, y) -> y;
        double[] ys_3 = ((double[])(explicit_euler(f_2, (double)(1.0), (double)(0.0), (double)(0.01), (double)(5.0))));
        System.out.println(ys_3[(int)((long)((long)(ys_3.length) - 1L))]);
    }
    public static void main(String[] args) {
        main();
    }

    static double[] appendDouble(double[] arr, double v) {
        double[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }
}
