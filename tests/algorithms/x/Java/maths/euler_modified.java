public class Main {

    static long ceil_float(double x) {
        long i = (long)(((Number)(x)).intValue());
        if ((double)(x) > (double)((((Number)(i)).doubleValue()))) {
            return (long)(i) + 1L;
        }
        return i;
    }

    static double exp_approx(double x) {
        double term = (double)(1.0);
        double sum_1 = (double)(1.0);
        long n_1 = 1L;
        while ((long)(n_1) < 20L) {
            term = (double)((double)((double)(term) * (double)(x)) / (double)((((Number)(n_1)).doubleValue())));
            sum_1 = (double)((double)(sum_1) + (double)(term));
            n_1 = (long)((long)(n_1) + 1L);
        }
        return sum_1;
    }

    static double[] euler_modified(java.util.function.BiFunction<Double,Double,Double> ode_func, double y0, double x0, double step, double x_end) {
        long n_2 = (long)(ceil_float((double)((double)(((double)(x_end) - (double)(x0))) / (double)(step))));
        double[] y_1 = ((double[])(new double[]{y0}));
        double x_1 = (double)(x0);
        long k_1 = 0L;
        while ((long)(k_1) < (long)(n_2)) {
            double y_predict_1 = (double)((double)(y_1[(int)((long)(k_1))]) + (double)((double)(step) * (double)(ode_func.apply((double)(x_1), (double)(y_1[(int)((long)(k_1))])))));
            double slope1_1 = (double)(ode_func.apply((double)(x_1), (double)(y_1[(int)((long)(k_1))])));
            double slope2_1 = (double)(ode_func.apply((double)((double)(x_1) + (double)(step)), (double)(y_predict_1)));
            double y_next_1 = (double)((double)(y_1[(int)((long)(k_1))]) + (double)((double)(((double)(step) / (double)(2.0))) * (double)(((double)(slope1_1) + (double)(slope2_1)))));
            y_1 = ((double[])(appendDouble(y_1, (double)(y_next_1))));
            x_1 = (double)((double)(x_1) + (double)(step));
            k_1 = (long)((long)(k_1) + 1L);
        }
        return y_1;
    }

    static double f1(double x, double y) {
        return (double)((double)((double)(-2.0) * (double)(x)) * (double)(y)) * (double)(y);
    }

    static double f2(double x, double y) {
        return (double)((double)(-2.0) * (double)(y)) + (double)((double)(((double)((double)(x) * (double)(x)) * (double)(x))) * (double)(exp_approx((double)((double)(-2.0) * (double)(x)))));
    }

    static void main() {
        double[] y1 = ((double[])(euler_modified(Main::f1, (double)(1.0), (double)(0.0), (double)(0.2), (double)(1.0))));
        System.out.println(y1[(int)((long)((long)(y1.length) - 1L))]);
        double[] y2_1 = ((double[])(euler_modified(Main::f2, (double)(1.0), (double)(0.0), (double)(0.1), (double)(0.3))));
        System.out.println(y2_1[(int)((long)((long)(y2_1.length) - 1L))]);
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
