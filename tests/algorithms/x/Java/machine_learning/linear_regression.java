public class Main {
    static double[][] data_x = ((double[][])(new double[][]{new double[]{1.0, 1.0}, new double[]{1.0, 2.0}, new double[]{1.0, 3.0}}));
    static double[] data_y = ((double[])(new double[]{1.0, 2.0, 3.0}));
    static double[] theta_2;
    static long i_10 = 0L;
    static double[] predicted_y;
    static double[] original_y = ((double[])(new double[]{2.5, 0.0, 2.0, 8.0}));
    static double mae;

    static double dot(double[] x, double[] y) {
        double sum = (double)(0.0);
        long i_1 = 0L;
        while ((long)(i_1) < (long)(x.length)) {
            sum = (double)((double)(sum) + (double)((double)(x[(int)((long)(i_1))]) * (double)(y[(int)((long)(i_1))])));
            i_1 = (long)((long)(i_1) + 1L);
        }
        return sum;
    }

    static double[] run_steep_gradient_descent(double[][] data_x, double[] data_y, long len_data, double alpha, double[] theta) {
        double[] gradients = ((double[])(new double[]{}));
        long j_1 = 0L;
        while ((long)(j_1) < (long)(theta.length)) {
            gradients = ((double[])(appendDouble(gradients, (double)(0.0))));
            j_1 = (long)((long)(j_1) + 1L);
        }
        long i_3 = 0L;
        while ((long)(i_3) < (long)(len_data)) {
            double prediction_1 = (double)(dot(((double[])(theta)), ((double[])(data_x[(int)((long)(i_3))]))));
            double error_1 = (double)((double)(prediction_1) - (double)(data_y[(int)((long)(i_3))]));
            long k_1 = 0L;
            while ((long)(k_1) < (long)(theta.length)) {
gradients[(int)((long)(k_1))] = (double)((double)(gradients[(int)((long)(k_1))]) + (double)((double)(error_1) * (double)(data_x[(int)((long)(i_3))][(int)((long)(k_1))])));
                k_1 = (long)((long)(k_1) + 1L);
            }
            i_3 = (long)((long)(i_3) + 1L);
        }
        double[] t_1 = ((double[])(new double[]{}));
        long g_1 = 0L;
        while ((long)(g_1) < (long)(theta.length)) {
            t_1 = ((double[])(appendDouble(t_1, (double)((double)(theta[(int)((long)(g_1))]) - (double)((double)(((double)(alpha) / (double)(len_data))) * (double)(gradients[(int)((long)(g_1))]))))));
            g_1 = (long)((long)(g_1) + 1L);
        }
        return t_1;
    }

    static double sum_of_square_error(double[][] data_x, double[] data_y, long len_data, double[] theta) {
        double total = (double)(0.0);
        long i_5 = 0L;
        while ((long)(i_5) < (long)(len_data)) {
            double prediction_3 = (double)(dot(((double[])(theta)), ((double[])(data_x[(int)((long)(i_5))]))));
            double diff_1 = (double)((double)(prediction_3) - (double)(data_y[(int)((long)(i_5))]));
            total = (double)((double)(total) + (double)((double)(diff_1) * (double)(diff_1)));
            i_5 = (long)((long)(i_5) + 1L);
        }
        return (double)(total) / (double)(((double)(2.0) * (double)(len_data)));
    }

    static double[] run_linear_regression(double[][] data_x, double[] data_y) {
        long iterations = 10L;
        double alpha_1 = (double)(0.01);
        long no_features_1 = (long)(data_x[(int)(0L)].length);
        long len_data_1 = (long)(data_x.length);
        double[] theta_1 = ((double[])(new double[]{}));
        long i_7 = 0L;
        while ((long)(i_7) < (long)(no_features_1)) {
            theta_1 = ((double[])(appendDouble(theta_1, (double)(0.0))));
            i_7 = (long)((long)(i_7) + 1L);
        }
        long iter_1 = 0L;
        while ((long)(iter_1) < (long)(iterations)) {
            theta_1 = ((double[])(run_steep_gradient_descent(((double[][])(data_x)), ((double[])(data_y)), (long)(len_data_1), (double)(alpha_1), ((double[])(theta_1)))));
            double error_3 = (double)(sum_of_square_error(((double[][])(data_x)), ((double[])(data_y)), (long)(len_data_1), ((double[])(theta_1))));
            System.out.println("At Iteration " + _p((long)(iter_1) + 1L) + " - Error is " + _p(error_3));
            iter_1 = (long)((long)(iter_1) + 1L);
        }
        return theta_1;
    }

    static double absf(double x) {
        if ((double)(x) < (double)(0.0)) {
            return -x;
        } else {
            return x;
        }
    }

    static double mean_absolute_error(double[] predicted_y, double[] original_y) {
        double total_1 = (double)(0.0);
        long i_9 = 0L;
        while ((long)(i_9) < (long)(predicted_y.length)) {
            double diff_3 = (double)(absf((double)((double)(predicted_y[(int)((long)(i_9))]) - (double)(original_y[(int)((long)(i_9))]))));
            total_1 = (double)((double)(total_1) + (double)(diff_3));
            i_9 = (long)((long)(i_9) + 1L);
        }
        return (double)(total_1) / (double)(predicted_y.length);
    }
    public static void main(String[] args) {
        theta_2 = ((double[])(run_linear_regression(((double[][])(data_x)), ((double[])(data_y)))));
        System.out.println("Resultant Feature vector :");
        while ((long)(i_10) < (long)(theta_2.length)) {
            System.out.println(_p(_getd(theta_2, ((Number)(i_10)).intValue())));
            i_10 = (long)((long)(i_10) + 1L);
        }
        predicted_y = ((double[])(new double[]{3.0, -0.5, 2.0, 7.0}));
        mae = (double)(mean_absolute_error(((double[])(predicted_y)), ((double[])(original_y))));
        System.out.println("Mean Absolute Error : " + _p(mae));
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
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }

    static Double _getd(double[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
