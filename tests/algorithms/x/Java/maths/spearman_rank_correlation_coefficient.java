public class Main {

    static int[] assign_ranks(double[] data) {
        int[] ranks = ((int[])(new int[]{}));
        int n = data.length;
        int i = 0;
        while (i < n) {
            int rank = 1;
            int j = 0;
            while (j < n) {
                if (data[j] < data[i] || (data[j] == data[i] && j < i)) {
                    rank = rank + 1;
                }
                j = j + 1;
            }
            ranks = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(ranks), java.util.stream.IntStream.of(rank)).toArray()));
            i = i + 1;
        }
        return ranks;
    }

    static double calculate_spearman_rank_correlation(double[] var1, double[] var2) {
        if (var1.length != var2.length) {
            throw new RuntimeException(String.valueOf("Lists must have equal length"));
        }
        int n_1 = var1.length;
        int[] rank1 = ((int[])(assign_ranks(((double[])(var1)))));
        int[] rank2 = ((int[])(assign_ranks(((double[])(var2)))));
        int i_1 = 0;
        double d_sq = 0.0;
        while (i_1 < n_1) {
            double diff = (((Number)((rank1[i_1] - rank2[i_1]))).doubleValue());
            d_sq = d_sq + diff * diff;
            i_1 = i_1 + 1;
        }
        double n_f = (((Number)(n_1)).doubleValue());
        return 1.0 - (6.0 * d_sq) / (n_f * (n_f * n_f - 1.0));
    }

    static void test_spearman() {
        double[] x = ((double[])(new double[]{1.0, 2.0, 3.0, 4.0, 5.0}));
        double[] y_inc = ((double[])(new double[]{2.0, 4.0, 6.0, 8.0, 10.0}));
        if (calculate_spearman_rank_correlation(((double[])(x)), ((double[])(y_inc))) != 1.0) {
            throw new RuntimeException(String.valueOf("case1"));
        }
        double[] y_dec = ((double[])(new double[]{5.0, 4.0, 3.0, 2.0, 1.0}));
        if (calculate_spearman_rank_correlation(((double[])(x)), ((double[])(y_dec))) != (-1.0)) {
            throw new RuntimeException(String.valueOf("case2"));
        }
        double[] y_mix = ((double[])(new double[]{5.0, 1.0, 2.0, 9.0, 5.0}));
        if (calculate_spearman_rank_correlation(((double[])(x)), ((double[])(y_mix))) != 0.6) {
            throw new RuntimeException(String.valueOf("case3"));
        }
    }

    static void main() {
        test_spearman();
        System.out.println(_p(calculate_spearman_rank_correlation(((double[])(new double[]{1.0, 2.0, 3.0, 4.0, 5.0})), ((double[])(new double[]{2.0, 4.0, 6.0, 8.0, 10.0})))));
        System.out.println(_p(calculate_spearman_rank_correlation(((double[])(new double[]{1.0, 2.0, 3.0, 4.0, 5.0})), ((double[])(new double[]{5.0, 4.0, 3.0, 2.0, 1.0})))));
        System.out.println(_p(calculate_spearman_rank_correlation(((double[])(new double[]{1.0, 2.0, 3.0, 4.0, 5.0})), ((double[])(new double[]{5.0, 1.0, 2.0, 9.0, 5.0})))));
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
        return String.valueOf(v);
    }
}
