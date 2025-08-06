public class Main {
    static double[][] data_x;
    static double[] data_y;
    static double[] theta_1;
    static int i_5 = 0;
    static double[] predicted_y;
    static double[] original_y;
    static double mae;

    static double dot(double[] x, double[] y) {
        double sum = 0.0;
        int i = 0;
        while (i < x.length) {
            sum = sum + x[i] * y[i];
            i = i + 1;
        }
        return sum;
    }

    static double[] run_steep_gradient_descent(double[][] data_x, double[] data_y, int len_data, double alpha, double[] theta) {
        double[] gradients = ((double[])(new double[]{}));
        int j = 0;
        while (j < theta.length) {
            gradients = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(gradients), java.util.stream.DoubleStream.of(0.0)).toArray()));
            j = j + 1;
        }
        int i_1 = 0;
        while (i_1 < len_data) {
            double prediction = dot(((double[])(theta)), ((double[])(data_x[i_1])));
            double error = prediction - data_y[i_1];
            int k = 0;
            while (k < theta.length) {
gradients[k] = gradients[k] + error * data_x[i_1][k];
                k = k + 1;
            }
            i_1 = i_1 + 1;
        }
        double[] t = ((double[])(new double[]{}));
        int g = 0;
        while (g < theta.length) {
            t = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(t), java.util.stream.DoubleStream.of(theta[g] - (alpha / len_data) * gradients[g])).toArray()));
            g = g + 1;
        }
        return t;
    }

    static double sum_of_square_error(double[][] data_x, double[] data_y, int len_data, double[] theta) {
        double total = 0.0;
        int i_2 = 0;
        while (i_2 < len_data) {
            double prediction_1 = dot(((double[])(theta)), ((double[])(data_x[i_2])));
            double diff = prediction_1 - data_y[i_2];
            total = total + diff * diff;
            i_2 = i_2 + 1;
        }
        return total / (2.0 * len_data);
    }

    static double[] run_linear_regression(double[][] data_x, double[] data_y) {
        int iterations = 10;
        double alpha = 0.01;
        int no_features = data_x[0].length;
        int len_data = data_x.length;
        double[] theta = ((double[])(new double[]{}));
        int i_3 = 0;
        while (i_3 < no_features) {
            theta = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(theta), java.util.stream.DoubleStream.of(0.0)).toArray()));
            i_3 = i_3 + 1;
        }
        int iter = 0;
        while (iter < iterations) {
            theta = ((double[])(run_steep_gradient_descent(((double[][])(data_x)), ((double[])(data_y)), len_data, alpha, ((double[])(theta)))));
            double error_1 = sum_of_square_error(((double[][])(data_x)), ((double[])(data_y)), len_data, ((double[])(theta)));
            System.out.println("At Iteration " + _p(iter + 1) + " - Error is " + _p(error_1));
            iter = iter + 1;
        }
        return theta;
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
        int i_4 = 0;
        while (i_4 < predicted_y.length) {
            double diff_1 = absf(predicted_y[i_4] - original_y[i_4]);
            total_1 = total_1 + diff_1;
            i_4 = i_4 + 1;
        }
        return total_1 / predicted_y.length;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            data_x = ((double[][])(new double[][]{new double[]{1.0, 1.0}, new double[]{1.0, 2.0}, new double[]{1.0, 3.0}}));
            data_y = ((double[])(new double[]{1.0, 2.0, 3.0}));
            theta_1 = ((double[])(run_linear_regression(((double[][])(data_x)), ((double[])(data_y)))));
            System.out.println("Resultant Feature vector :");
            i_5 = 0;
            while (i_5 < theta_1.length) {
                System.out.println(_p(_geto(theta_1, i_5)));
                i_5 = i_5 + 1;
            }
            predicted_y = ((double[])(new double[]{3.0, -0.5, 2.0, 7.0}));
            original_y = ((double[])(new double[]{2.5, 0.0, 2.0, 8.0}));
            mae = mean_absolute_error(((double[])(predicted_y)), ((double[])(original_y)));
            System.out.println("Mean Absolute Error : " + _p(mae));
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
