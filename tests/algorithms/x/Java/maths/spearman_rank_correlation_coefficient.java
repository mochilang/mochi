public class Main {

    static long[] assign_ranks(double[] data) {
        long[] ranks = ((long[])(new long[]{}));
        long n_1 = (long)(data.length);
        long i_1 = 0L;
        while ((long)(i_1) < (long)(n_1)) {
            long rank_1 = 1L;
            long j_1 = 0L;
            while ((long)(j_1) < (long)(n_1)) {
                if ((double)(data[(int)((long)(j_1))]) < (double)(data[(int)((long)(i_1))]) || ((double)(data[(int)((long)(j_1))]) == (double)(data[(int)((long)(i_1))]) && (long)(j_1) < (long)(i_1))) {
                    rank_1 = (long)((long)(rank_1) + 1L);
                }
                j_1 = (long)((long)(j_1) + 1L);
            }
            ranks = ((long[])(appendLong(ranks, (long)(rank_1))));
            i_1 = (long)((long)(i_1) + 1L);
        }
        return ranks;
    }

    static double calculate_spearman_rank_correlation(double[] var1, double[] var2) {
        if ((long)(var1.length) != (long)(var2.length)) {
            throw new RuntimeException(String.valueOf("Lists must have equal length"));
        }
        long n_3 = (long)(var1.length);
        long[] rank1_1 = ((long[])(assign_ranks(((double[])(var1)))));
        long[] rank2_1 = ((long[])(assign_ranks(((double[])(var2)))));
        long i_3 = 0L;
        double d_sq_1 = (double)(0.0);
        while ((long)(i_3) < (long)(n_3)) {
            double diff_1 = (double)((((Number)(((long)(rank1_1[(int)((long)(i_3))]) - (long)(rank2_1[(int)((long)(i_3))])))).doubleValue()));
            d_sq_1 = (double)((double)(d_sq_1) + (double)((double)(diff_1) * (double)(diff_1)));
            i_3 = (long)((long)(i_3) + 1L);
        }
        double n_f_1 = (double)((((Number)(n_3)).doubleValue()));
        return (double)(1.0) - (double)((double)(((double)(6.0) * (double)(d_sq_1))) / (double)(((double)(n_f_1) * (double)(((double)((double)(n_f_1) * (double)(n_f_1)) - (double)(1.0))))));
    }

    static void test_spearman() {
        double[] x = ((double[])(new double[]{1.0, 2.0, 3.0, 4.0, 5.0}));
        double[] y_inc_1 = ((double[])(new double[]{2.0, 4.0, 6.0, 8.0, 10.0}));
        if ((double)(calculate_spearman_rank_correlation(((double[])(x)), ((double[])(y_inc_1)))) != (double)(1.0)) {
            throw new RuntimeException(String.valueOf("case1"));
        }
        double[] y_dec_1 = ((double[])(new double[]{5.0, 4.0, 3.0, 2.0, 1.0}));
        if ((double)(calculate_spearman_rank_correlation(((double[])(x)), ((double[])(y_dec_1)))) != (double)((-1.0))) {
            throw new RuntimeException(String.valueOf("case2"));
        }
        double[] y_mix_1 = ((double[])(new double[]{5.0, 1.0, 2.0, 9.0, 5.0}));
        if ((double)(calculate_spearman_rank_correlation(((double[])(x)), ((double[])(y_mix_1)))) != (double)(0.6)) {
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

    static long[] appendLong(long[] arr, long v) {
        long[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
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
}
