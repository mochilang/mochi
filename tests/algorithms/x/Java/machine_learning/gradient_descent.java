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
        double value = params[0];
        int i = 0;
        while (i < input.length) {
            value = value + input[i] * params[i + 1];
            i = i + 1;
        }
        return value;
    }

    static double calc_error(DataPoint dp, double[] params) {
        return hypothesis_value(((double[])(dp.x)), ((double[])(params))) - dp.y;
    }

    static double summation_of_cost_derivative(int index, double[] params, DataPoint[] data) {
        double sum = 0.0;
        int i_1 = 0;
        while (i_1 < data.length) {
            DataPoint dp = data[i_1];
            double e = calc_error(dp, ((double[])(params)));
            if (index == (-1)) {
                sum = sum + e;
            } else {
                sum = sum + e * dp.x[index];
            }
            i_1 = i_1 + 1;
        }
        return sum;
    }

    static double get_cost_derivative(int index, double[] params, DataPoint[] data) {
        return summation_of_cost_derivative(index, ((double[])(params)), ((DataPoint[])(data))) / (((Number)(data.length)).doubleValue());
    }

    static boolean allclose(double[] a, double[] b, double atol, double rtol) {
        int i_2 = 0;
        while (i_2 < a.length) {
            double diff = absf(a[i_2] - b[i_2]);
            double limit = atol + rtol * absf(b[i_2]);
            if (diff > limit) {
                return false;
            }
            i_2 = i_2 + 1;
        }
        return true;
    }

    static double[] run_gradient_descent(DataPoint[] train_data, double[] initial_params) {
        double learning_rate = 0.009;
        double absolute_error_limit = 2e-06;
        double relative_error_limit = 0.0;
        int j = 0;
        double[] params = ((double[])(initial_params));
        while (true) {
            j = j + 1;
            double[] temp = ((double[])(new double[]{}));
            int i_3 = 0;
            while (i_3 < params.length) {
                double deriv = get_cost_derivative(i_3 - 1, ((double[])(params)), ((DataPoint[])(train_data)));
                temp = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(temp), java.util.stream.DoubleStream.of(params[i_3] - learning_rate * deriv)).toArray()));
                i_3 = i_3 + 1;
            }
            if (((Boolean)(allclose(((double[])(params)), ((double[])(temp)), absolute_error_limit, relative_error_limit)))) {
                System.out.println("Number of iterations:" + _p(j));
                break;
            }
            params = ((double[])(temp));
        }
        return params;
    }

    static void test_gradient_descent(DataPoint[] test_data, double[] params) {
        int i_4 = 0;
        while (i_4 < test_data.length) {
            DataPoint dp_1 = test_data[i_4];
            System.out.println("Actual output value:" + _p(dp_1.y));
            System.out.println("Hypothesis output:" + _p(hypothesis_value(((double[])(dp_1.x)), ((double[])(params)))));
            i_4 = i_4 + 1;
        }
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            train_data = ((DataPoint[])(new DataPoint[]{new DataPoint(new double[]{5.0, 2.0, 3.0}, 15.0), new DataPoint(new double[]{6.0, 5.0, 9.0}, 25.0), new DataPoint(new double[]{11.0, 12.0, 13.0}, 41.0), new DataPoint(new double[]{1.0, 1.0, 1.0}, 8.0), new DataPoint(new double[]{11.0, 12.0, 13.0}, 41.0)}));
            test_data = ((DataPoint[])(new DataPoint[]{new DataPoint(new double[]{515.0, 22.0, 13.0}, 555.0), new DataPoint(new double[]{61.0, 35.0, 49.0}, 150.0)}));
            parameter_vector = ((double[])(new double[]{2.0, 4.0, 1.0, 5.0}));
            parameter_vector = ((double[])(run_gradient_descent(((DataPoint[])(train_data)), ((double[])(parameter_vector)))));
            System.out.println("\nTesting gradient descent for a linear hypothesis function.\n");
            test_gradient_descent(((DataPoint[])(test_data)), ((double[])(parameter_vector)));
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
}
