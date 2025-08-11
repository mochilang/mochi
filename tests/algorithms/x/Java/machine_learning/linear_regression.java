public class Main {
    static double[][] data_x;
    static double[] data_y;
    static double[] theta_2;
    static long i_10 = 0;
    static double[] predicted_y;
    static double[] original_y;
    static double mae;

    static double dot(double[] x, double[] y) {
        double sum = 0.0;
        long i_1 = 0;
        while (i_1 < x.length) {
            sum = sum + x[(int)(i_1)] * y[(int)(i_1)];
            i_1 = i_1 + 1;
        }
        return sum;
    }

    static double[] run_steep_gradient_descent(double[][] data_x, double[] data_y, long len_data, double alpha, double[] theta) {
        double[] gradients = ((double[])(new double[]{}));
        long j_1 = 0;
        while (j_1 < theta.length) {
            gradients = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(gradients), java.util.stream.DoubleStream.of(0.0)).toArray()));
            j_1 = j_1 + 1;
        }
        long i_3 = 0;
        while (i_3 < len_data) {
            double prediction_1 = dot(((double[])(theta)), ((double[])(data_x[(int)(i_3)])));
            double error_1 = prediction_1 - data_y[(int)(i_3)];
            long k_1 = 0;
            while (k_1 < theta.length) {
gradients[(int)(k_1)] = gradients[(int)(k_1)] + error_1 * data_x[(int)(i_3)][(int)(k_1)];
                k_1 = k_1 + 1;
            }
            i_3 = i_3 + 1;
        }
        double[] t_1 = ((double[])(new double[]{}));
        long g_1 = 0;
        while (g_1 < theta.length) {
            t_1 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(t_1), java.util.stream.DoubleStream.of(theta[(int)(g_1)] - (alpha / len_data) * gradients[(int)(g_1)])).toArray()));
            g_1 = g_1 + 1;
        }
        return t_1;
    }

    static double sum_of_square_error(double[][] data_x, double[] data_y, long len_data, double[] theta) {
        double total = 0.0;
        long i_5 = 0;
        while (i_5 < len_data) {
            double prediction_3 = dot(((double[])(theta)), ((double[])(data_x[(int)(i_5)])));
            double diff_1 = prediction_3 - data_y[(int)(i_5)];
            total = total + diff_1 * diff_1;
            i_5 = i_5 + 1;
        }
        return total / (2.0 * len_data);
    }

    static double[] run_linear_regression(double[][] data_x, double[] data_y) {
        long iterations = 10;
        double alpha_1 = 0.01;
        long no_features_1 = data_x[(int)(0)].length;
        long len_data_1 = data_x.length;
        double[] theta_1 = ((double[])(new double[]{}));
        long i_7 = 0;
        while (i_7 < no_features_1) {
            theta_1 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(theta_1), java.util.stream.DoubleStream.of(0.0)).toArray()));
            i_7 = i_7 + 1;
        }
        long iter_1 = 0;
        while (iter_1 < iterations) {
            theta_1 = ((double[])(run_steep_gradient_descent(((double[][])(data_x)), ((double[])(data_y)), len_data_1, alpha_1, ((double[])(theta_1)))));
            double error_3 = sum_of_square_error(((double[][])(data_x)), ((double[])(data_y)), len_data_1, ((double[])(theta_1)));
            System.out.println("At Iteration " + _p(iter_1 + 1) + " - Error is " + _p(error_3));
            iter_1 = iter_1 + 1;
        }
        return theta_1;
    }

    static double absf(double x) {
        if (x < 0.0) {
            return -x;
        } else {
            return x;
        }
    }

    static double mean_absolute_error(double[] predicted_y, double[] original_y) {
        double total_1 = 0.0;
        long i_9 = 0;
        while (i_9 < predicted_y.length) {
            double diff_3 = absf(predicted_y[(int)(i_9)] - original_y[(int)(i_9)]);
            total_1 = total_1 + diff_3;
            i_9 = i_9 + 1;
        }
        return total_1 / predicted_y.length;
    }
    public static void main(String[] args) {
        data_x = ((double[][])(new double[][]{new double[]{1.0, 1.0}, new double[]{1.0, 2.0}, new double[]{1.0, 3.0}}));
        data_y = ((double[])(new double[]{1.0, 2.0, 3.0}));
        theta_2 = ((double[])(run_linear_regression(((double[][])(data_x)), ((double[])(data_y)))));
        System.out.println("Resultant Feature vector :");
        i_10 = 0;
        while (i_10 < theta_2.length) {
            System.out.println(_p(_getd(theta_2, ((Number)(i_10)).intValue())));
            i_10 = i_10 + 1;
        }
        predicted_y = ((double[])(new double[]{3.0, -0.5, 2.0, 7.0}));
        original_y = ((double[])(new double[]{2.5, 0.0, 2.0, 8.0}));
        mae = mean_absolute_error(((double[])(predicted_y)), ((double[])(original_y)));
        System.out.println("Mean Absolute Error : " + _p(mae));
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
