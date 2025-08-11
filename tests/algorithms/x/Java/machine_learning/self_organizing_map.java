public class Main {

    static long get_winner(double[][] weights, long[] sample) {
        double d0 = 0.0;
        double d1_1 = 0.0;
        for (int i = 0; i < sample.length; i++) {
            double diff0_1 = sample[(int)(i)] - weights[(int)(0)][(int)(i)];
            double diff1_1 = sample[(int)(i)] - weights[(int)(1)][(int)(i)];
            d0 = d0 + diff0_1 * diff0_1;
            d1_1 = d1_1 + diff1_1 * diff1_1;
            return d0 > d1_1 ? 0 : 1;
        }
        return 0;
    }

    static double[][] update(double[][] weights, long[] sample, long j, double alpha) {
        for (int i = 0; i < weights.length; i++) {
weights[(int)(j)][(int)(i)] = weights[(int)(j)][(int)(i)] + alpha * (sample[(int)(i)] - weights[(int)(j)][(int)(i)]);
        }
        return weights;
    }

    static String list_to_string(double[] xs) {
        String s = "[";
        long i_1 = 0;
        while (i_1 < xs.length) {
            s = s + _p(_getd(xs, ((Number)(i_1)).intValue()));
            if (i_1 < xs.length - 1) {
                s = s + ", ";
            }
            i_1 = i_1 + 1;
        }
        s = s + "]";
        return s;
    }

    static String matrix_to_string(double[][] m) {
        String s_1 = "[";
        long i_3 = 0;
        while (i_3 < m.length) {
            s_1 = s_1 + String.valueOf(list_to_string(((double[])(m[(int)(i_3)]))));
            if (i_3 < m.length - 1) {
                s_1 = s_1 + ", ";
            }
            i_3 = i_3 + 1;
        }
        s_1 = s_1 + "]";
        return s_1;
    }

    static void main() {
        long[][] training_samples = ((long[][])(new long[][]{new long[]{1, 1, 0, 0}, new long[]{0, 0, 0, 1}, new long[]{1, 0, 0, 0}, new long[]{0, 0, 1, 1}}));
        double[][] weights_1 = ((double[][])(new double[][]{new double[]{0.2, 0.6, 0.5, 0.9}, new double[]{0.8, 0.4, 0.7, 0.3}}));
        long epochs_1 = 3;
        double alpha_1 = 0.5;
        for (int _v = 0; _v < epochs_1; _v++) {
            for (int j = 0; j < training_samples.length; j++) {
                long[] sample_1 = ((long[])(training_samples[(int)(j)]));
                long winner_1 = get_winner(((double[][])(weights_1)), ((long[])(sample_1)));
                weights_1 = ((double[][])(update(((double[][])(weights_1)), ((long[])(sample_1)), winner_1, alpha_1)));
            }
        }
        long[] sample_3 = ((long[])(new long[]{0, 0, 0, 1}));
        long winner_3 = get_winner(((double[][])(weights_1)), ((long[])(sample_3)));
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
            if (d == Math.rint(d)) return String.valueOf((long) d);
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }

    static Double _getd(double[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
