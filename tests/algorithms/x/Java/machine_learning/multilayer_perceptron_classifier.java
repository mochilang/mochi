public class Main {
    static double[][] X = ((double[][])(new double[][]{new double[]{0.0, 0.0}, new double[]{1.0, 1.0}, new double[]{1.0, 0.0}, new double[]{0.0, 1.0}}));
    static double[] Y = ((double[])(new double[]{0.0, 1.0, 0.0, 0.0}));
    static double[][] test_data = ((double[][])(new double[][]{new double[]{0.0, 0.0}, new double[]{0.0, 1.0}, new double[]{1.0, 1.0}}));
    static double[][] w1 = new double[0][];
    static double[] b1 = ((double[])(new double[]{0.0, 0.0}));
    static double[] w2 = new double[0];
    static double b2 = (double)(0.0);
    static long[] preds_1;

    static double exp_taylor(double x) {
        double term = (double)(1.0);
        double sum_1 = (double)(1.0);
        double i_1 = (double)(1.0);
        while ((double)(i_1) < (double)(20.0)) {
            term = (double)((double)((double)(term) * (double)(x)) / (double)(i_1));
            sum_1 = (double)((double)(sum_1) + (double)(term));
            i_1 = (double)((double)(i_1) + (double)(1.0));
        }
        return sum_1;
    }

    static double sigmoid(double x) {
        return (double)(1.0) / (double)(((double)(1.0) + (double)(exp_taylor((double)(-x)))));
    }

    static void train(long epochs, double lr) {
        long e = 0L;
        while ((long)(e) < (long)(epochs)) {
            long i_3 = 0L;
            while ((long)(i_3) < (long)(X.length)) {
                double x0_1 = (double)(X[(int)((long)(i_3))][(int)(0L)]);
                double x1_1 = (double)(X[(int)((long)(i_3))][(int)(1L)]);
                double target_1 = (double)(Y[(int)((long)(i_3))]);
                double z1_1 = (double)((double)((double)((double)(w1[(int)(0L)][(int)(0L)]) * (double)(x0_1)) + (double)((double)(w1[(int)(1L)][(int)(0L)]) * (double)(x1_1))) + (double)(b1[(int)(0L)]));
                double z2_1 = (double)((double)((double)((double)(w1[(int)(0L)][(int)(1L)]) * (double)(x0_1)) + (double)((double)(w1[(int)(1L)][(int)(1L)]) * (double)(x1_1))) + (double)(b1[(int)(1L)]));
                double h1_1 = (double)(sigmoid((double)(z1_1)));
                double h2_1 = (double)(sigmoid((double)(z2_1)));
                double z3_1 = (double)((double)((double)((double)(w2[(int)(0L)]) * (double)(h1_1)) + (double)((double)(w2[(int)(1L)]) * (double)(h2_1))) + (double)(b2));
                double out_1 = (double)(sigmoid((double)(z3_1)));
                double error_1 = (double)((double)(out_1) - (double)(target_1));
                double d1_1 = (double)((double)((double)((double)(h1_1) * (double)(((double)(1.0) - (double)(h1_1)))) * (double)(w2[(int)(0L)])) * (double)(error_1));
                double d2_1 = (double)((double)((double)((double)(h2_1) * (double)(((double)(1.0) - (double)(h2_1)))) * (double)(w2[(int)(1L)])) * (double)(error_1));
w2[(int)(0L)] = (double)((double)(w2[(int)(0L)]) - (double)((double)((double)(lr) * (double)(error_1)) * (double)(h1_1)));
w2[(int)(1L)] = (double)((double)(w2[(int)(1L)]) - (double)((double)((double)(lr) * (double)(error_1)) * (double)(h2_1)));
                b2 = (double)((double)(b2) - (double)((double)(lr) * (double)(error_1)));
w1[(int)(0L)][(int)(0L)] = (double)((double)(w1[(int)(0L)][(int)(0L)]) - (double)((double)((double)(lr) * (double)(d1_1)) * (double)(x0_1)));
w1[(int)(1L)][(int)(0L)] = (double)((double)(w1[(int)(1L)][(int)(0L)]) - (double)((double)((double)(lr) * (double)(d1_1)) * (double)(x1_1)));
b1[(int)(0L)] = (double)((double)(b1[(int)(0L)]) - (double)((double)(lr) * (double)(d1_1)));
w1[(int)(0L)][(int)(1L)] = (double)((double)(w1[(int)(0L)][(int)(1L)]) - (double)((double)((double)(lr) * (double)(d2_1)) * (double)(x0_1)));
w1[(int)(1L)][(int)(1L)] = (double)((double)(w1[(int)(1L)][(int)(1L)]) - (double)((double)((double)(lr) * (double)(d2_1)) * (double)(x1_1)));
b1[(int)(1L)] = (double)((double)(b1[(int)(1L)]) - (double)((double)(lr) * (double)(d2_1)));
                i_3 = (long)((long)(i_3) + 1L);
            }
            e = (long)((long)(e) + 1L);
        }
    }

    static long[] predict(double[][] samples) {
        long[] preds = ((long[])(new long[]{}));
        long i_5 = 0L;
        while ((long)(i_5) < (long)(samples.length)) {
            double x0_3 = (double)(samples[(int)((long)(i_5))][(int)(0L)]);
            double x1_3 = (double)(samples[(int)((long)(i_5))][(int)(1L)]);
            double z1_3 = (double)((double)((double)((double)(w1[(int)(0L)][(int)(0L)]) * (double)(x0_3)) + (double)((double)(w1[(int)(1L)][(int)(0L)]) * (double)(x1_3))) + (double)(b1[(int)(0L)]));
            double z2_3 = (double)((double)((double)((double)(w1[(int)(0L)][(int)(1L)]) * (double)(x0_3)) + (double)((double)(w1[(int)(1L)][(int)(1L)]) * (double)(x1_3))) + (double)(b1[(int)(1L)]));
            double h1_3 = (double)(sigmoid((double)(z1_3)));
            double h2_3 = (double)(sigmoid((double)(z2_3)));
            double z3_3 = (double)((double)((double)((double)(w2[(int)(0L)]) * (double)(h1_3)) + (double)((double)(w2[(int)(1L)]) * (double)(h2_3))) + (double)(b2));
            double out_3 = (double)(sigmoid((double)(z3_3)));
            long label_1 = 0L;
            if ((double)(out_3) >= (double)(0.5)) {
                label_1 = 1L;
            }
            preds = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(preds), java.util.stream.LongStream.of((long)(label_1))).toArray()));
            i_5 = (long)((long)(i_5) + 1L);
        }
        return preds;
    }

    static long[] wrapper(long[] y) {
        return y;
    }
    public static void main(String[] args) {
        w1 = ((double[][])(new double[][]{new double[]{0.5, -0.5}, new double[]{0.5, 0.5}}));
        w2 = ((double[])(new double[]{0.5, -0.5}));
        train(4000L, (double)(0.5));
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
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
