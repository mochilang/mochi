public class Main {

    static double int_to_float(long x) {
        return (double)(x) * (double)(1.0);
    }

    static double abs_float(double x) {
        if ((double)(x) < (double)(0.0)) {
            return (double)(0.0) - (double)(x);
        }
        return x;
    }

    static double exp_approx(double x) {
        double term = (double)(1.0);
        double sum_1 = (double)(1.0);
        long i_1 = 1L;
        while ((long)(i_1) < 10L) {
            term = (double)((double)((double)(term) * (double)(x)) / (double)(int_to_float((long)(i_1))));
            sum_1 = (double)((double)(sum_1) + (double)(term));
            i_1 = (long)((long)(i_1) + 1L);
        }
        return sum_1;
    }

    static long floor_int(double x) {
        long i_2 = 0L;
        while ((double)(int_to_float((long)((long)(i_2) + 1L))) <= (double)(x)) {
            i_2 = (long)((long)(i_2) + 1L);
        }
        return i_2;
    }

    static double dot(double[] a, double[] b) {
        double s = (double)(0.0);
        long i_4 = 0L;
        while ((long)(i_4) < (long)(a.length)) {
            s = (double)((double)(s) + (double)((double)(a[(int)((long)(i_4))]) * (double)(b[(int)((long)(i_4))])));
            i_4 = (long)((long)(i_4) + 1L);
        }
        return s;
    }

    static double[][] transpose(double[][] m) {
        long rows = (long)(m.length);
        long cols_1 = (long)(m[(int)(0L)].length);
        double[][] res_1 = ((double[][])(new double[][]{}));
        long j_1 = 0L;
        while ((long)(j_1) < (long)(cols_1)) {
            double[] row_1 = ((double[])(new double[]{}));
            long i_6 = 0L;
            while ((long)(i_6) < (long)(rows)) {
                row_1 = ((double[])(appendDouble(row_1, (double)(m[(int)((long)(i_6))][(int)((long)(j_1))]))));
                i_6 = (long)((long)(i_6) + 1L);
            }
            res_1 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(res_1), java.util.stream.Stream.of(new double[][]{row_1})).toArray(double[][]::new)));
            j_1 = (long)((long)(j_1) + 1L);
        }
        return res_1;
    }

    static double[][] matmul(double[][] a, double[][] b) {
        long n = (long)(a.length);
        long m_1 = (long)(b[(int)(0L)].length);
        long p_1 = (long)(b.length);
        double[][] res_3 = ((double[][])(new double[][]{}));
        long i_8 = 0L;
        while ((long)(i_8) < (long)(n)) {
            double[] row_3 = ((double[])(new double[]{}));
            long j_3 = 0L;
            while ((long)(j_3) < (long)(m_1)) {
                double s_2 = (double)(0.0);
                long k_1 = 0L;
                while ((long)(k_1) < (long)(p_1)) {
                    s_2 = (double)((double)(s_2) + (double)((double)(a[(int)((long)(i_8))][(int)((long)(k_1))]) * (double)(b[(int)((long)(k_1))][(int)((long)(j_3))])));
                    k_1 = (long)((long)(k_1) + 1L);
                }
                row_3 = ((double[])(appendDouble(row_3, (double)(s_2))));
                j_3 = (long)((long)(j_3) + 1L);
            }
            res_3 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(res_3), java.util.stream.Stream.of(new double[][]{row_3})).toArray(double[][]::new)));
            i_8 = (long)((long)(i_8) + 1L);
        }
        return res_3;
    }

    static double[] matvec(double[][] a, double[] b) {
        double[] res_4 = ((double[])(new double[]{}));
        long i_10 = 0L;
        while ((long)(i_10) < (long)(a.length)) {
            res_4 = ((double[])(appendDouble(res_4, (double)(dot(((double[])(a[(int)((long)(i_10))])), ((double[])(b)))))));
            i_10 = (long)((long)(i_10) + 1L);
        }
        return res_4;
    }

    static double[][] identity(long n) {
        double[][] res_5 = ((double[][])(new double[][]{}));
        long i_12 = 0L;
        while ((long)(i_12) < (long)(n)) {
            double[] row_5 = ((double[])(new double[]{}));
            long j_5 = 0L;
            while ((long)(j_5) < (long)(n)) {
                row_5 = ((double[])(appendDouble(row_5, (double)((long)(i_12) == (long)(j_5) ? 1.0 : 0.0))));
                j_5 = (long)((long)(j_5) + 1L);
            }
            res_5 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(res_5), java.util.stream.Stream.of(new double[][]{row_5})).toArray(double[][]::new)));
            i_12 = (long)((long)(i_12) + 1L);
        }
        return res_5;
    }

    static double[][] invert(double[][] mat) {
        long n_1 = (long)(mat.length);
        double[][] a_1 = ((double[][])(mat));
        double[][] inv_1 = ((double[][])(identity((long)(n_1))));
        long i_14 = 0L;
        while ((long)(i_14) < (long)(n_1)) {
            double pivot_1 = (double)(a_1[(int)((long)(i_14))][(int)((long)(i_14))]);
            long j_7 = 0L;
            while ((long)(j_7) < (long)(n_1)) {
a_1[(int)((long)(i_14))][(int)((long)(j_7))] = (double)((double)(a_1[(int)((long)(i_14))][(int)((long)(j_7))]) / (double)(pivot_1));
inv_1[(int)((long)(i_14))][(int)((long)(j_7))] = (double)((double)(inv_1[(int)((long)(i_14))][(int)((long)(j_7))]) / (double)(pivot_1));
                j_7 = (long)((long)(j_7) + 1L);
            }
            long k_3 = 0L;
            while ((long)(k_3) < (long)(n_1)) {
                if ((long)(k_3) != (long)(i_14)) {
                    double factor_1 = (double)(a_1[(int)((long)(k_3))][(int)((long)(i_14))]);
                    j_7 = 0L;
                    while ((long)(j_7) < (long)(n_1)) {
a_1[(int)((long)(k_3))][(int)((long)(j_7))] = (double)((double)(a_1[(int)((long)(k_3))][(int)((long)(j_7))]) - (double)((double)(factor_1) * (double)(a_1[(int)((long)(i_14))][(int)((long)(j_7))])));
inv_1[(int)((long)(k_3))][(int)((long)(j_7))] = (double)((double)(inv_1[(int)((long)(k_3))][(int)((long)(j_7))]) - (double)((double)(factor_1) * (double)(inv_1[(int)((long)(i_14))][(int)((long)(j_7))])));
                        j_7 = (long)((long)(j_7) + 1L);
                    }
                }
                k_3 = (long)((long)(k_3) + 1L);
            }
            i_14 = (long)((long)(i_14) + 1L);
        }
        return inv_1;
    }

    static double[] normal_equation(double[][] X, double[] y) {
        double[][] Xt = ((double[][])(transpose(((double[][])(X)))));
        double[][] XtX_1 = ((double[][])(matmul(((double[][])(Xt)), ((double[][])(X)))));
        double[][] XtX_inv_1 = ((double[][])(invert(((double[][])(XtX_1)))));
        double[] Xty_1 = ((double[])(matvec(((double[][])(Xt)), ((double[])(y)))));
        return matvec(((double[][])(XtX_inv_1)), ((double[])(Xty_1)));
    }

    static double linear_regression_prediction(double[] train_dt, double[] train_usr, double[] train_mtch, double[] test_dt, double[] test_mtch) {
        double[][] X = ((double[][])(new double[][]{}));
        long i_16 = 0L;
        while ((long)(i_16) < (long)(train_dt.length)) {
            X = java.util.stream.Stream.concat(java.util.Arrays.stream(X), java.util.stream.Stream.of(new Object[][]{new double[]{1.0, train_dt[(int)((long)(i_16))], train_mtch[(int)((long)(i_16))]}})).toArray(Object[][]::new);
            i_16 = (long)((long)(i_16) + 1L);
        }
        double[] beta_1 = ((double[])(normal_equation(((double[][])(X)), ((double[])(train_usr)))));
        return abs_float((double)((double)((double)(beta_1[(int)(0L)]) + (double)((double)(test_dt[(int)(0L)]) * (double)(beta_1[(int)(1L)]))) + (double)((double)(test_mtch[(int)(0L)]) * (double)(beta_1[(int)(2L)]))));
    }

    static double sarimax_predictor(double[] train_user, double[] train_match, double[] test_match) {
        long n_2 = (long)(train_user.length);
        double[][] X_2 = ((double[][])(new double[][]{}));
        double[] y_1 = ((double[])(new double[]{}));
        long i_18 = 1L;
        while ((long)(i_18) < (long)(n_2)) {
            X_2 = java.util.stream.Stream.concat(java.util.Arrays.stream(X_2), java.util.stream.Stream.of(new Object[][]{new double[]{1.0, train_user[(int)((long)((long)(i_18) - 1L))], train_match[(int)((long)(i_18))]}})).toArray(Object[][]::new);
            y_1 = ((double[])(appendDouble(y_1, (double)(train_user[(int)((long)(i_18))]))));
            i_18 = (long)((long)(i_18) + 1L);
        }
        double[] beta_3 = ((double[])(normal_equation(((double[][])(X_2)), ((double[])(y_1)))));
        return (double)((double)(beta_3[(int)(0L)]) + (double)((double)(beta_3[(int)(1L)]) * (double)(train_user[(int)((long)((long)(n_2) - 1L))]))) + (double)((double)(beta_3[(int)(2L)]) * (double)(test_match[(int)(0L)]));
    }

    static double rbf_kernel(double[] a, double[] b, double gamma) {
        double sum_2 = (double)(0.0);
        long i_20 = 0L;
        while ((long)(i_20) < (long)(a.length)) {
            double diff_1 = (double)((double)(a[(int)((long)(i_20))]) - (double)(b[(int)((long)(i_20))]));
            sum_2 = (double)((double)(sum_2) + (double)((double)(diff_1) * (double)(diff_1)));
            i_20 = (long)((long)(i_20) + 1L);
        }
        return exp_approx((double)((double)(-gamma) * (double)(sum_2)));
    }

    static double support_vector_regressor(double[][] x_train, double[][] x_test, double[] train_user) {
        double gamma = (double)(0.1);
        double[] weights_1 = ((double[])(new double[]{}));
        long i_22 = 0L;
        while ((long)(i_22) < (long)(x_train.length)) {
            weights_1 = ((double[])(appendDouble(weights_1, (double)(rbf_kernel(((double[])(x_train[(int)((long)(i_22))])), ((double[])(x_test[(int)(0L)])), (double)(gamma))))));
            i_22 = (long)((long)(i_22) + 1L);
        }
        double num_1 = (double)(0.0);
        double den_1 = (double)(0.0);
        i_22 = 0L;
        while ((long)(i_22) < (long)(train_user.length)) {
            num_1 = (double)((double)(num_1) + (double)((double)(weights_1[(int)((long)(i_22))]) * (double)(train_user[(int)((long)(i_22))])));
            den_1 = (double)((double)(den_1) + (double)(weights_1[(int)((long)(i_22))]));
            i_22 = (long)((long)(i_22) + 1L);
        }
        return (double)(num_1) / (double)(den_1);
    }

    static double[] set_at_float(double[] xs, long idx, double value) {
        long i_23 = 0L;
        double[] res_7 = ((double[])(new double[]{}));
        while ((long)(i_23) < (long)(xs.length)) {
            if ((long)(i_23) == (long)(idx)) {
                res_7 = ((double[])(appendDouble(res_7, (double)(value))));
            } else {
                res_7 = ((double[])(appendDouble(res_7, (double)(xs[(int)((long)(i_23))]))));
            }
            i_23 = (long)((long)(i_23) + 1L);
        }
        return res_7;
    }

    static double[] sort_float(double[] xs) {
        double[] res_8 = ((double[])(xs));
        long i_25 = 1L;
        while ((long)(i_25) < (long)(res_8.length)) {
            double key_1 = (double)(res_8[(int)((long)(i_25))]);
            long j_9 = (long)((long)(i_25) - 1L);
            while ((long)(j_9) >= 0L && (double)(res_8[(int)((long)(j_9))]) > (double)(key_1)) {
                res_8 = ((double[])(set_at_float(((double[])(res_8)), (long)((long)(j_9) + 1L), (double)(res_8[(int)((long)(j_9))]))));
                j_9 = (long)((long)(j_9) - 1L);
            }
            res_8 = ((double[])(set_at_float(((double[])(res_8)), (long)((long)(j_9) + 1L), (double)(key_1))));
            i_25 = (long)((long)(i_25) + 1L);
        }
        return res_8;
    }

    static double percentile(double[] data, double q) {
        double[] sorted = ((double[])(sort_float(((double[])(data)))));
        long n_4 = (long)(sorted.length);
        double pos_1 = (double)((double)(((double)(q) / (double)(100.0))) * (double)(int_to_float((long)((long)(n_4) - 1L))));
        long idx_1 = (long)(floor_int((double)(pos_1)));
        double frac_1 = (double)((double)(pos_1) - (double)(int_to_float((long)(idx_1))));
        if ((long)((long)(idx_1) + 1L) < (long)(n_4)) {
            return (double)((double)(sorted[(int)((long)(idx_1))]) * (double)(((double)(1.0) - (double)(frac_1)))) + (double)((double)(sorted[(int)((long)((long)(idx_1) + 1L))]) * (double)(frac_1));
        }
        return sorted[(int)((long)(idx_1))];
    }

    static double interquartile_range_checker(double[] train_user) {
        double q1 = (double)(percentile(((double[])(train_user)), (double)(25.0)));
        double q3_1 = (double)(percentile(((double[])(train_user)), (double)(75.0)));
        double iqr_1 = (double)((double)(q3_1) - (double)(q1));
        return (double)(q1) - (double)((double)(iqr_1) * (double)(0.1));
    }

    static boolean data_safety_checker(double[] list_vote, double actual_result) {
        long safe = 0L;
        long not_safe_1 = 0L;
        long i_27 = 0L;
        while ((long)(i_27) < (long)(list_vote.length)) {
            double v_1 = (double)(list_vote[(int)((long)(i_27))]);
            if ((double)(v_1) > (double)(actual_result)) {
                safe = (long)((long)(not_safe_1) + 1L);
            } else             if ((double)(abs_float((double)((double)(abs_float((double)(v_1))) - (double)(abs_float((double)(actual_result)))))) <= (double)(0.1)) {
                safe = (long)((long)(safe) + 1L);
            } else {
                not_safe_1 = (long)((long)(not_safe_1) + 1L);
            }
            i_27 = (long)((long)(i_27) + 1L);
        }
        return (long)(safe) > (long)(not_safe_1);
    }

    static void main() {
        double[] vote = ((double[])(new double[]{linear_regression_prediction(((double[])(new double[]{2.0, 3.0, 4.0, 5.0})), ((double[])(new double[]{5.0, 3.0, 4.0, 6.0})), ((double[])(new double[]{3.0, 1.0, 2.0, 4.0})), ((double[])(new double[]{2.0})), ((double[])(new double[]{2.0}))), sarimax_predictor(((double[])(new double[]{4.0, 2.0, 6.0, 8.0})), ((double[])(new double[]{3.0, 1.0, 2.0, 4.0})), ((double[])(new double[]{2.0}))), support_vector_regressor(((double[][])(new double[][]{new double[]{5.0, 2.0}, new double[]{1.0, 5.0}, new double[]{6.0, 2.0}})), ((double[][])(new double[][]{new double[]{3.0, 2.0}})), ((double[])(new double[]{2.0, 1.0, 4.0})))}));
        System.out.println(vote[(int)(0L)]);
        System.out.println(vote[(int)(1L)]);
        System.out.println(vote[(int)(2L)]);
        System.out.println(data_safety_checker(((double[])(vote)), (double)(5.0)));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            main();
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

    static double[] appendDouble(double[] arr, double v) {
        double[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }
}
