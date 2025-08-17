public class Main {

    static double floor(double x) {
        long i = (long)(((Number)(x)).intValue());
        if ((double)((((Number)(i)).doubleValue())) > (double)(x)) {
            i = (long)((long)(i) - 1L);
        }
        return ((Number)(i)).doubleValue();
    }

    static double pow10(long n) {
        double p = (double)(1.0);
        long i_2 = 0L;
        while ((long)(i_2) < (long)(n)) {
            p = (double)((double)(p) * (double)(10.0));
            i_2 = (long)((long)(i_2) + 1L);
        }
        return p;
    }

    static double round(double x, long n) {
        double m = (double)(pow10((long)(n)));
        return Math.floor((double)((double)(x) * (double)(m)) + (double)(0.5)) / (double)(m);
    }

    static double[][] clone_matrix(double[][] mat) {
        double[][] new_mat = ((double[][])(new double[][]{}));
        long i_4 = 0L;
        while ((long)(i_4) < (long)(mat.length)) {
            double[] row_1 = ((double[])(new double[]{}));
            long j_1 = 0L;
            while ((long)(j_1) < (long)(mat[(int)((long)(i_4))].length)) {
                row_1 = ((double[])(appendDouble(row_1, (double)(mat[(int)((long)(i_4))][(int)((long)(j_1))]))));
                j_1 = (long)((long)(j_1) + 1L);
            }
            new_mat = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(new_mat), java.util.stream.Stream.of(new double[][]{row_1})).toArray(double[][]::new)));
            i_4 = (long)((long)(i_4) + 1L);
        }
        return new_mat;
    }

    static double[] solve_simultaneous(double[][] equations) {
        long n = (long)(equations.length);
        if ((long)(n) == 0L) {
            throw new RuntimeException(String.valueOf("solve_simultaneous() requires n lists of length n+1"));
        }
        long m_2 = (long)((long)(n) + 1L);
        long i_6 = 0L;
        while ((long)(i_6) < (long)(n)) {
            if ((long)(equations[(int)((long)(i_6))].length) != (long)(m_2)) {
                throw new RuntimeException(String.valueOf("solve_simultaneous() requires n lists of length n+1"));
            }
            i_6 = (long)((long)(i_6) + 1L);
        }
        double[][] a_1 = ((double[][])(clone_matrix(((double[][])(equations)))));
        long row_3 = 0L;
        while ((long)(row_3) < (long)(n)) {
            long pivot_1 = (long)(row_3);
            while ((long)(pivot_1) < (long)(n) && (double)(a_1[(int)((long)(pivot_1))][(int)((long)(row_3))]) == (double)(0.0)) {
                pivot_1 = (long)((long)(pivot_1) + 1L);
            }
            if ((long)(pivot_1) == (long)(n)) {
                throw new RuntimeException(String.valueOf("solve_simultaneous() requires at least 1 full equation"));
            }
            if ((long)(pivot_1) != (long)(row_3)) {
                double[] temp_1 = ((double[])(a_1[(int)((long)(row_3))]));
a_1[(int)((long)(row_3))] = ((double[])(a_1[(int)((long)(pivot_1))]));
a_1[(int)((long)(pivot_1))] = ((double[])(temp_1));
            }
            double pivot_val_1 = (double)(a_1[(int)((long)(row_3))][(int)((long)(row_3))]);
            long col_1 = 0L;
            while ((long)(col_1) < (long)(m_2)) {
a_1[(int)((long)(row_3))][(int)((long)(col_1))] = (double)((double)(a_1[(int)((long)(row_3))][(int)((long)(col_1))]) / (double)(pivot_val_1));
                col_1 = (long)((long)(col_1) + 1L);
            }
            long r_1 = 0L;
            while ((long)(r_1) < (long)(n)) {
                if ((long)(r_1) != (long)(row_3)) {
                    double factor_1 = (double)(a_1[(int)((long)(r_1))][(int)((long)(row_3))]);
                    long c_1 = 0L;
                    while ((long)(c_1) < (long)(m_2)) {
a_1[(int)((long)(r_1))][(int)((long)(c_1))] = (double)((double)(a_1[(int)((long)(r_1))][(int)((long)(c_1))]) - (double)((double)(factor_1) * (double)(a_1[(int)((long)(row_3))][(int)((long)(c_1))])));
                        c_1 = (long)((long)(c_1) + 1L);
                    }
                }
                r_1 = (long)((long)(r_1) + 1L);
            }
            row_3 = (long)((long)(row_3) + 1L);
        }
        double[] res_1 = ((double[])(new double[]{}));
        long k_1 = 0L;
        while ((long)(k_1) < (long)(n)) {
            res_1 = ((double[])(appendDouble(res_1, (double)(round((double)(a_1[(int)((long)(k_1))][(int)((long)((long)(m_2) - 1L))]), 5L)))));
            k_1 = (long)((long)(k_1) + 1L);
        }
        return res_1;
    }

    static void test_solver() {
        double[][] a_2 = ((double[][])(new double[][]{new double[]{1.0, 2.0, 3.0}, new double[]{4.0, 5.0, 6.0}}));
        double[] r1_1 = ((double[])(solve_simultaneous(((double[][])(a_2)))));
        if (!((long)(r1_1.length) == 2L && (double)(r1_1[(int)(0L)]) == (double)(((double)(0.0) - (double)(1.0))) && (double)(r1_1[(int)(1L)]) == (double)(2.0))) {
            throw new RuntimeException(String.valueOf("test1 failed"));
        }
        double[][] b_1 = ((double[][])(new double[][]{new double[]{0.0, ((double)(0.0) - (double)(3.0)), 1.0, 7.0}, new double[]{3.0, 2.0, ((double)(0.0) - (double)(1.0)), 11.0}, new double[]{5.0, 1.0, ((double)(0.0) - (double)(2.0)), 12.0}}));
        double[] r2_1 = ((double[])(solve_simultaneous(((double[][])(b_1)))));
        if (!((long)(r2_1.length) == 3L && (double)(r2_1[(int)(0L)]) == (double)(6.4) && (double)(r2_1[(int)(1L)]) == (double)(1.2) && (double)(r2_1[(int)(2L)]) == (double)(10.6))) {
            throw new RuntimeException(String.valueOf("test2 failed"));
        }
    }

    static void main() {
        test_solver();
        double[][] eq_1 = ((double[][])(new double[][]{new double[]{2.0, 1.0, 1.0, 1.0, 1.0, 4.0}, new double[]{1.0, 2.0, 1.0, 1.0, 1.0, 5.0}, new double[]{1.0, 1.0, 2.0, 1.0, 1.0, 6.0}, new double[]{1.0, 1.0, 1.0, 2.0, 1.0, 7.0}, new double[]{1.0, 1.0, 1.0, 1.0, 2.0, 8.0}}));
        System.out.println(_p(solve_simultaneous(((double[][])(eq_1)))));
        System.out.println(_p(solve_simultaneous(((double[][])(new double[][]{new double[]{4.0, 2.0}})))));
    }
    public static void main(String[] args) {
        main();
    }

    static double[] appendDouble(double[] arr, double v) {
        double[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
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
