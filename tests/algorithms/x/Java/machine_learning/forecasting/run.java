public class Main {

    static double int_to_float(long x) {
        return x * 1.0;
    }

    static double abs_float(double x) {
        if (x < 0.0) {
            return 0.0 - x;
        }
        return x;
    }

    static double exp_approx(double x) {
        double term = 1.0;
        double sum_1 = 1.0;
        long i_1 = 1;
        while (i_1 < 10) {
            term = term * x / int_to_float(i_1);
            sum_1 = sum_1 + term;
            i_1 = i_1 + 1;
        }
        return sum_1;
    }

    static long floor_int(double x) {
        long i_2 = 0;
        while (int_to_float(i_2 + 1) <= x) {
            i_2 = i_2 + 1;
        }
        return i_2;
    }

    static double dot(double[] a, double[] b) {
        double s = 0.0;
        long i_4 = 0;
        while (i_4 < a.length) {
            s = s + a[(int)(i_4)] * b[(int)(i_4)];
            i_4 = i_4 + 1;
        }
        return s;
    }

    static double[][] transpose(double[][] m) {
        long rows = m.length;
        long cols_1 = m[(int)(0)].length;
        double[][] res_1 = ((double[][])(new double[][]{}));
        long j_1 = 0;
        while (j_1 < cols_1) {
            double[] row_1 = ((double[])(new double[]{}));
            long i_6 = 0;
            while (i_6 < rows) {
                row_1 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row_1), java.util.stream.DoubleStream.of(m[(int)(i_6)][(int)(j_1)])).toArray()));
                i_6 = i_6 + 1;
            }
            res_1 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(res_1), java.util.stream.Stream.of(row_1)).toArray(double[][]::new)));
            j_1 = j_1 + 1;
        }
        return res_1;
    }

    static double[][] matmul(double[][] a, double[][] b) {
        long n = a.length;
        long m_1 = b[(int)(0)].length;
        long p_1 = b.length;
        double[][] res_3 = ((double[][])(new double[][]{}));
        long i_8 = 0;
        while (i_8 < n) {
            double[] row_3 = ((double[])(new double[]{}));
            long j_3 = 0;
            while (j_3 < m_1) {
                double s_2 = 0.0;
                long k_1 = 0;
                while (k_1 < p_1) {
                    s_2 = s_2 + a[(int)(i_8)][(int)(k_1)] * b[(int)(k_1)][(int)(j_3)];
                    k_1 = k_1 + 1;
                }
                row_3 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row_3), java.util.stream.DoubleStream.of(s_2)).toArray()));
                j_3 = j_3 + 1;
            }
            res_3 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(res_3), java.util.stream.Stream.of(row_3)).toArray(double[][]::new)));
            i_8 = i_8 + 1;
        }
        return res_3;
    }

    static double[] matvec(double[][] a, double[] b) {
        double[] res_4 = ((double[])(new double[]{}));
        long i_10 = 0;
        while (i_10 < a.length) {
            res_4 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(res_4), java.util.stream.DoubleStream.of(dot(((double[])(a[(int)(i_10)])), ((double[])(b))))).toArray()));
            i_10 = i_10 + 1;
        }
        return res_4;
    }

    static double[][] identity(long n) {
        double[][] res_5 = ((double[][])(new double[][]{}));
        long i_12 = 0;
        while (i_12 < n) {
            double[] row_5 = ((double[])(new double[]{}));
            long j_5 = 0;
            while (j_5 < n) {
                row_5 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row_5), java.util.stream.DoubleStream.of(i_12 == j_5 ? 1.0 : 0.0)).toArray()));
                j_5 = j_5 + 1;
            }
            res_5 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(res_5), java.util.stream.Stream.of(row_5)).toArray(double[][]::new)));
            i_12 = i_12 + 1;
        }
        return res_5;
    }

    static double[][] invert(double[][] mat) {
        long n_1 = mat.length;
        double[][] a_1 = ((double[][])(mat));
        double[][] inv_1 = ((double[][])(identity(n_1)));
        long i_14 = 0;
        while (i_14 < n_1) {
            double pivot_1 = a_1[(int)(i_14)][(int)(i_14)];
            long j_7 = 0;
            while (j_7 < n_1) {
a_1[(int)(i_14)][(int)(j_7)] = a_1[(int)(i_14)][(int)(j_7)] / pivot_1;
inv_1[(int)(i_14)][(int)(j_7)] = inv_1[(int)(i_14)][(int)(j_7)] / pivot_1;
                j_7 = j_7 + 1;
            }
            long k_3 = 0;
            while (k_3 < n_1) {
                if (k_3 != i_14) {
                    double factor_1 = a_1[(int)(k_3)][(int)(i_14)];
                    j_7 = 0;
                    while (j_7 < n_1) {
a_1[(int)(k_3)][(int)(j_7)] = a_1[(int)(k_3)][(int)(j_7)] - factor_1 * a_1[(int)(i_14)][(int)(j_7)];
inv_1[(int)(k_3)][(int)(j_7)] = inv_1[(int)(k_3)][(int)(j_7)] - factor_1 * inv_1[(int)(i_14)][(int)(j_7)];
                        j_7 = j_7 + 1;
                    }
                }
                k_3 = k_3 + 1;
            }
            i_14 = i_14 + 1;
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
        long i_16 = 0;
        while (i_16 < train_dt.length) {
            X = java.util.stream.Stream.concat(java.util.Arrays.stream(X), java.util.stream.Stream.of(new Object[]{1.0, train_dt[(int)(i_16)], train_mtch[(int)(i_16)]})).toArray(Object[][]::new);
            i_16 = i_16 + 1;
        }
        double[] beta_1 = ((double[])(normal_equation(((double[][])(X)), ((double[])(train_usr)))));
        return abs_float(beta_1[(int)(0)] + test_dt[(int)(0)] * beta_1[(int)(1)] + test_mtch[(int)(0)] * beta_1[(int)(2)]);
    }

    static double sarimax_predictor(double[] train_user, double[] train_match, double[] test_match) {
        long n_2 = train_user.length;
        double[][] X_2 = ((double[][])(new double[][]{}));
        double[] y_1 = ((double[])(new double[]{}));
        long i_18 = 1;
        while (i_18 < n_2) {
            X_2 = java.util.stream.Stream.concat(java.util.Arrays.stream(X_2), java.util.stream.Stream.of(new Object[]{1.0, train_user[(int)(i_18 - 1)], train_match[(int)(i_18)]})).toArray(Object[][]::new);
            y_1 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(y_1), java.util.stream.DoubleStream.of(train_user[(int)(i_18)])).toArray()));
            i_18 = i_18 + 1;
        }
        double[] beta_3 = ((double[])(normal_equation(((double[][])(X_2)), ((double[])(y_1)))));
        return beta_3[(int)(0)] + beta_3[(int)(1)] * train_user[(int)(n_2 - 1)] + beta_3[(int)(2)] * test_match[(int)(0)];
    }

    static double rbf_kernel(double[] a, double[] b, double gamma) {
        double sum_2 = 0.0;
        long i_20 = 0;
        while (i_20 < a.length) {
            double diff_1 = a[(int)(i_20)] - b[(int)(i_20)];
            sum_2 = sum_2 + diff_1 * diff_1;
            i_20 = i_20 + 1;
        }
        return exp_approx(-gamma * sum_2);
    }

    static double support_vector_regressor(double[][] x_train, double[][] x_test, double[] train_user) {
        double gamma = 0.1;
        double[] weights_1 = ((double[])(new double[]{}));
        long i_22 = 0;
        while (i_22 < x_train.length) {
            weights_1 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(weights_1), java.util.stream.DoubleStream.of(rbf_kernel(((double[])(x_train[(int)(i_22)])), ((double[])(x_test[(int)(0)])), gamma))).toArray()));
            i_22 = i_22 + 1;
        }
        double num_1 = 0.0;
        double den_1 = 0.0;
        i_22 = 0;
        while (i_22 < train_user.length) {
            num_1 = num_1 + weights_1[(int)(i_22)] * train_user[(int)(i_22)];
            den_1 = den_1 + weights_1[(int)(i_22)];
            i_22 = i_22 + 1;
        }
        return num_1 / den_1;
    }

    static double[] set_at_float(double[] xs, long idx, double value) {
        long i_23 = 0;
        double[] res_7 = ((double[])(new double[]{}));
        while (i_23 < xs.length) {
            if (i_23 == idx) {
                res_7 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(res_7), java.util.stream.DoubleStream.of(value)).toArray()));
            } else {
                res_7 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(res_7), java.util.stream.DoubleStream.of(xs[(int)(i_23)])).toArray()));
            }
            i_23 = i_23 + 1;
        }
        return res_7;
    }

    static double[] sort_float(double[] xs) {
        double[] res_8 = ((double[])(xs));
        long i_25 = 1;
        while (i_25 < res_8.length) {
            double key_1 = res_8[(int)(i_25)];
            long j_9 = i_25 - 1;
            while (j_9 >= 0 && res_8[(int)(j_9)] > key_1) {
                res_8 = ((double[])(set_at_float(((double[])(res_8)), j_9 + 1, res_8[(int)(j_9)])));
                j_9 = j_9 - 1;
            }
            res_8 = ((double[])(set_at_float(((double[])(res_8)), j_9 + 1, key_1)));
            i_25 = i_25 + 1;
        }
        return res_8;
    }

    static double percentile(double[] data, double q) {
        double[] sorted = ((double[])(sort_float(((double[])(data)))));
        long n_4 = sorted.length;
        double pos_1 = (q / 100.0) * int_to_float(n_4 - 1);
        long idx_1 = floor_int(pos_1);
        double frac_1 = pos_1 - int_to_float(idx_1);
        if (idx_1 + 1 < n_4) {
            return sorted[(int)(idx_1)] * (1.0 - frac_1) + sorted[(int)(idx_1 + 1)] * frac_1;
        }
        return sorted[(int)(idx_1)];
    }

    static double interquartile_range_checker(double[] train_user) {
        double q1 = percentile(((double[])(train_user)), 25.0);
        double q3_1 = percentile(((double[])(train_user)), 75.0);
        double iqr_1 = q3_1 - q1;
        return q1 - iqr_1 * 0.1;
    }

    static boolean data_safety_checker(double[] list_vote, double actual_result) {
        long safe = 0;
        long not_safe_1 = 0;
        long i_27 = 0;
        while (i_27 < list_vote.length) {
            double v_1 = list_vote[(int)(i_27)];
            if (v_1 > actual_result) {
                safe = not_safe_1 + 1;
            } else             if (abs_float(abs_float(v_1) - abs_float(actual_result)) <= 0.1) {
                safe = safe + 1;
            } else {
                not_safe_1 = not_safe_1 + 1;
            }
            i_27 = i_27 + 1;
        }
        return safe > not_safe_1;
    }

    static void main() {
        double[] vote = ((double[])(new double[]{linear_regression_prediction(((double[])(new double[]{2.0, 3.0, 4.0, 5.0})), ((double[])(new double[]{5.0, 3.0, 4.0, 6.0})), ((double[])(new double[]{3.0, 1.0, 2.0, 4.0})), ((double[])(new double[]{2.0})), ((double[])(new double[]{2.0}))), sarimax_predictor(((double[])(new double[]{4.0, 2.0, 6.0, 8.0})), ((double[])(new double[]{3.0, 1.0, 2.0, 4.0})), ((double[])(new double[]{2.0}))), support_vector_regressor(((double[][])(new double[][]{new double[]{5.0, 2.0}, new double[]{1.0, 5.0}, new double[]{6.0, 2.0}})), ((double[][])(new double[][]{new double[]{3.0, 2.0}})), ((double[])(new double[]{2.0, 1.0, 4.0})))}));
        System.out.println(vote[(int)(0)]);
        System.out.println(vote[(int)(1)]);
        System.out.println(vote[(int)(2)]);
        System.out.println(data_safety_checker(((double[])(vote)), 5.0));
    }
    public static void main(String[] args) {
        main();
    }
}
