public class Main {
    static class DataPoint {
        double[] x;
        double y;
        DataPoint(double[] x, double y) {
            this.x = x;
            this.y = y;
        }
        DataPoint() {}
        @Override public String toString() {
            return String.format("{'x': %s, 'y': %s}", String.valueOf(x), String.valueOf(y));
        }
    }

    static DataPoint[] train_data;
    static DataPoint[] test_data;
    static double[] parameter_vector = new double[0];

    static double absf(double x) {
        if (x < 0.0) {
            return -x;
        }
        return x;
    }

    static double hypothesis_value(double[] input, double[] params) {
        double value = params[(int)(0)];
        long i_1 = 0;
        while (i_1 < input.length) {
            value = value + input[(int)(i_1)] * params[(int)(i_1 + 1)];
            i_1 = i_1 + 1;
        }
        return value;
    }

    static double calc_error(DataPoint dp, double[] params) {
        return hypothesis_value(((double[])(dp.x)), ((double[])(params))) - dp.y;
    }

    static double summation_of_cost_derivative(long index, double[] params, DataPoint[] data) {
        double sum = 0.0;
        long i_3 = 0;
        while (i_3 < data.length) {
            DataPoint dp_1 = data[(int)(i_3)];
            double e_1 = calc_error(dp_1, ((double[])(params)));
            if (index == (-1)) {
                sum = sum + e_1;
            } else {
                sum = sum + e_1 * dp_1.x[(int)(index)];
            }
            i_3 = i_3 + 1;
        }
        return sum;
    }

    static double get_cost_derivative(long index, double[] params, DataPoint[] data) {
        return summation_of_cost_derivative(index, ((double[])(params)), ((DataPoint[])(data))) / (((Number)(data.length)).doubleValue());
    }

    static boolean allclose(double[] a, double[] b, double atol, double rtol) {
        long i_4 = 0;
        while (i_4 < a.length) {
            double diff_1 = absf(a[(int)(i_4)] - b[(int)(i_4)]);
            double limit_1 = atol + rtol * absf(b[(int)(i_4)]);
            if (diff_1 > limit_1) {
                return false;
            }
            i_4 = i_4 + 1;
        }
        return true;
    }

    static double[] run_gradient_descent(DataPoint[] train_data, double[] initial_params) {
        double learning_rate = 0.009;
        double absolute_error_limit_1 = 2e-06;
        double relative_error_limit_1 = 0.0;
        long j_1 = 0;
        double[] params_1 = ((double[])(initial_params));
        while (true) {
            j_1 = j_1 + 1;
            double[] temp_1 = ((double[])(new double[]{}));
            long i_6 = 0;
            while (i_6 < params_1.length) {
                double deriv_1 = get_cost_derivative(i_6 - 1, ((double[])(params_1)), ((DataPoint[])(train_data)));
                temp_1 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(temp_1), java.util.stream.DoubleStream.of(params_1[(int)(i_6)] - learning_rate * deriv_1)).toArray()));
                i_6 = i_6 + 1;
            }
            if (((Boolean)(allclose(((double[])(params_1)), ((double[])(temp_1)), absolute_error_limit_1, relative_error_limit_1)))) {
                System.out.println("Number of iterations:" + _p(j_1));
                break;
            }
            params_1 = ((double[])(temp_1));
        }
        return params_1;
    }

    static void test_gradient_descent(DataPoint[] test_data, double[] params) {
        long i_7 = 0;
        while (i_7 < test_data.length) {
            DataPoint dp_3 = test_data[(int)(i_7)];
            System.out.println("Actual output value:" + _p(dp_3.y));
            System.out.println("Hypothesis output:" + _p(hypothesis_value(((double[])(dp_3.x)), ((double[])(params)))));
            i_7 = i_7 + 1;
        }
    }
    public static void main(String[] args) {
        train_data = ((DataPoint[])(new DataPoint[]{new DataPoint(new double[]{5.0, 2.0, 3.0}, 15.0), new DataPoint(new double[]{6.0, 5.0, 9.0}, 25.0), new DataPoint(new double[]{11.0, 12.0, 13.0}, 41.0), new DataPoint(new double[]{1.0, 1.0, 1.0}, 8.0), new DataPoint(new double[]{11.0, 12.0, 13.0}, 41.0)}));
        test_data = ((DataPoint[])(new DataPoint[]{new DataPoint(new double[]{515.0, 22.0, 13.0}, 555.0), new DataPoint(new double[]{61.0, 35.0, 49.0}, 150.0)}));
        parameter_vector = ((double[])(new double[]{2.0, 4.0, 1.0, 5.0}));
        parameter_vector = ((double[])(run_gradient_descent(((DataPoint[])(train_data)), ((double[])(parameter_vector)))));
        System.out.println("\nTesting gradient descent for a linear hypothesis function.\n");
        test_gradient_descent(((DataPoint[])(test_data)), ((double[])(parameter_vector)));
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
}
