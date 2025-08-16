public class Main {

    static long get_winner(double[][] weights, long[] sample) {
        double d0 = (double)(0.0);
        double d1_1 = (double)(0.0);
        for (int i = 0; i < sample.length; i++) {
            double diff0_1 = (double)((double)(sample[(int)((long)(i))]) - (double)(weights[(int)(0L)][(int)((long)(i))]));
            double diff1_1 = (double)((double)(sample[(int)((long)(i))]) - (double)(weights[(int)(1L)][(int)((long)(i))]));
            d0 = (double)((double)(d0) + (double)((double)(diff0_1) * (double)(diff0_1)));
            d1_1 = (double)((double)(d1_1) + (double)((double)(diff1_1) * (double)(diff1_1)));
            return (double)(d0) > (double)(d1_1) ? 0 : 1;
        }
        return 0;
    }

    static double[][] update(double[][] weights, long[] sample, long j, double alpha) {
        for (int i = 0; i < weights.length; i++) {
weights[(int)((long)(j))][(int)((long)(i))] = (double)((double)(weights[(int)((long)(j))][(int)((long)(i))]) + (double)((double)(alpha) * (double)(((double)(sample[(int)((long)(i))]) - (double)(weights[(int)((long)(j))][(int)((long)(i))])))));
        }
        return weights;
    }

    static String list_to_string(double[] xs) {
        String s = "[";
        long i_1 = 0L;
        while ((long)(i_1) < (long)(xs.length)) {
            s = s + _p(_getd(xs, ((Number)(i_1)).intValue()));
            if ((long)(i_1) < (long)((long)(xs.length) - 1L)) {
                s = s + ", ";
            }
            i_1 = (long)((long)(i_1) + 1L);
        }
        s = s + "]";
        return s;
    }

    static String matrix_to_string(double[][] m) {
        String s_1 = "[";
        long i_3 = 0L;
        while ((long)(i_3) < (long)(m.length)) {
            s_1 = s_1 + String.valueOf(list_to_string(((double[])(m[(int)((long)(i_3))]))));
            if ((long)(i_3) < (long)((long)(m.length) - 1L)) {
                s_1 = s_1 + ", ";
            }
            i_3 = (long)((long)(i_3) + 1L);
        }
        s_1 = s_1 + "]";
        return s_1;
    }

    static void main() {
        long[][] training_samples = ((long[][])(new long[][]{new long[]{1, 1, 0, 0}, new long[]{0, 0, 0, 1}, new long[]{1, 0, 0, 0}, new long[]{0, 0, 1, 1}}));
        double[][] weights_1 = ((double[][])(new double[][]{new double[]{0.2, 0.6, 0.5, 0.9}, new double[]{0.8, 0.4, 0.7, 0.3}}));
        long epochs_1 = 3L;
        double alpha_1 = (double)(0.5);
        for (int _v = 0; _v < epochs_1; _v++) {
            for (int j = 0; j < training_samples.length; j++) {
                long[] sample_1 = ((long[])(training_samples[(int)((long)(j))]));
                long winner_1 = (long)(get_winner(((double[][])(weights_1)), ((long[])(sample_1))));
                weights_1 = ((double[][])(update(((double[][])(weights_1)), ((long[])(sample_1)), (long)(winner_1), (double)(alpha_1))));
            }
        }
        long[] sample_3 = ((long[])(new long[]{0, 0, 0, 1}));
        long winner_3 = (long)(get_winner(((double[][])(weights_1)), ((long[])(sample_3))));
        System.out.println("Clusters that the test sample belongs to : " + _p(winner_3));
        System.out.println("Weights that have been trained : " + String.valueOf(matrix_to_string(((double[][])(weights_1)))));
    }
    public static void main(String[] args) {
        main();
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
