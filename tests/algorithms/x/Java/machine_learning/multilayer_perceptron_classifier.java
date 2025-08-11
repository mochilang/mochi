public class Main {
    static double[][] X;
    static double[] Y;
    static double[][] test_data;
    static double[][] w1 = new double[0][];
    static double[] b1 = new double[0];
    static double[] w2 = new double[0];
    static double b2 = 0;
    static long[] preds_1;

    static double exp_taylor(double x) {
        double term = 1.0;
        double sum_1 = 1.0;
        double i_1 = 1.0;
        while (i_1 < 20.0) {
            term = term * x / i_1;
            sum_1 = sum_1 + term;
            i_1 = i_1 + 1.0;
        }
        return sum_1;
    }

    static double sigmoid(double x) {
        return 1.0 / (1.0 + exp_taylor(-x));
    }

    static void train(long epochs, double lr) {
        long e = 0;
        while (e < epochs) {
            long i_3 = 0;
            while (i_3 < X.length) {
                double x0_1 = X[(int)(i_3)][(int)(0)];
                double x1_1 = X[(int)(i_3)][(int)(1)];
                double target_1 = Y[(int)(i_3)];
                double z1_1 = w1[(int)(0)][(int)(0)] * x0_1 + w1[(int)(1)][(int)(0)] * x1_1 + b1[(int)(0)];
                double z2_1 = w1[(int)(0)][(int)(1)] * x0_1 + w1[(int)(1)][(int)(1)] * x1_1 + b1[(int)(1)];
                double h1_1 = sigmoid(z1_1);
                double h2_1 = sigmoid(z2_1);
                double z3_1 = w2[(int)(0)] * h1_1 + w2[(int)(1)] * h2_1 + b2;
                double out_1 = sigmoid(z3_1);
                double error_1 = out_1 - target_1;
                double d1_1 = h1_1 * (1.0 - h1_1) * w2[(int)(0)] * error_1;
                double d2_1 = h2_1 * (1.0 - h2_1) * w2[(int)(1)] * error_1;
w2[(int)(0)] = w2[(int)(0)] - lr * error_1 * h1_1;
w2[(int)(1)] = w2[(int)(1)] - lr * error_1 * h2_1;
                b2 = b2 - lr * error_1;
w1[(int)(0)][(int)(0)] = w1[(int)(0)][(int)(0)] - lr * d1_1 * x0_1;
w1[(int)(1)][(int)(0)] = w1[(int)(1)][(int)(0)] - lr * d1_1 * x1_1;
b1[(int)(0)] = b1[(int)(0)] - lr * d1_1;
w1[(int)(0)][(int)(1)] = w1[(int)(0)][(int)(1)] - lr * d2_1 * x0_1;
w1[(int)(1)][(int)(1)] = w1[(int)(1)][(int)(1)] - lr * d2_1 * x1_1;
b1[(int)(1)] = b1[(int)(1)] - lr * d2_1;
                i_3 = i_3 + 1;
            }
            e = e + 1;
        }
    }

    static long[] predict(double[][] samples) {
        long[] preds = ((long[])(new long[]{}));
        long i_5 = 0;
        while (i_5 < samples.length) {
            double x0_3 = samples[(int)(i_5)][(int)(0)];
            double x1_3 = samples[(int)(i_5)][(int)(1)];
            double z1_3 = w1[(int)(0)][(int)(0)] * x0_3 + w1[(int)(1)][(int)(0)] * x1_3 + b1[(int)(0)];
            double z2_3 = w1[(int)(0)][(int)(1)] * x0_3 + w1[(int)(1)][(int)(1)] * x1_3 + b1[(int)(1)];
            double h1_3 = sigmoid(z1_3);
            double h2_3 = sigmoid(z2_3);
            double z3_3 = w2[(int)(0)] * h1_3 + w2[(int)(1)] * h2_3 + b2;
            double out_3 = sigmoid(z3_3);
            long label_1 = 0;
            if (out_3 >= 0.5) {
                label_1 = 1;
            }
            preds = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(preds), java.util.stream.LongStream.of(label_1)).toArray()));
            i_5 = i_5 + 1;
        }
        return preds;
    }

    static long[] wrapper(long[] y) {
        return y;
    }
    public static void main(String[] args) {
        X = ((double[][])(new double[][]{new double[]{0.0, 0.0}, new double[]{1.0, 1.0}, new double[]{1.0, 0.0}, new double[]{0.0, 1.0}}));
        Y = ((double[])(new double[]{0.0, 1.0, 0.0, 0.0}));
        test_data = ((double[][])(new double[][]{new double[]{0.0, 0.0}, new double[]{0.0, 1.0}, new double[]{1.0, 1.0}}));
        w1 = ((double[][])(new double[][]{new double[]{0.5, -0.5}, new double[]{0.5, 0.5}}));
        b1 = ((double[])(new double[]{0.0, 0.0}));
        w2 = ((double[])(new double[]{0.5, -0.5}));
        b2 = 0.0;
        train(4000, 0.5);
        preds_1 = ((long[])(wrapper(((long[])(predict(((double[][])(test_data))))))));
        System.out.println(_p(preds_1));
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
