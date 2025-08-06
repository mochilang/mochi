public class Main {

    static double int_to_float(int x) {
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
        double sum = 1.0;
        int i = 1;
        while (i < 10) {
            term = term * x / int_to_float(i);
            sum = sum + term;
            i = i + 1;
        }
        return sum;
    }

    static int floor_int(double x) {
        int i_1 = 0;
        while (int_to_float(i_1 + 1) <= x) {
            i_1 = i_1 + 1;
        }
        return i_1;
    }

    static double dot(double[] a, double[] b) {
        double s = 0.0;
        int i_2 = 0;
        while (i_2 < a.length) {
            s = s + a[i_2] * b[i_2];
            i_2 = i_2 + 1;
        }
        return s;
    }

    static double[][] transpose(double[][] m) {
        int rows = m.length;
        int cols = m[0].length;
        double[][] res = ((double[][])(new double[][]{}));
        int j = 0;
        while (j < cols) {
            double[] row = ((double[])(new double[]{}));
            int i_3 = 0;
            while (i_3 < rows) {
                row = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row), java.util.stream.DoubleStream.of(m[i_3][j])).toArray()));
                i_3 = i_3 + 1;
            }
            res = ((double[][])(appendObj(res, row)));
            j = j + 1;
        }
        return res;
    }

    static double[][] matmul(double[][] a, double[][] b) {
        int n = a.length;
        int m = b[0].length;
        int p = b.length;
        double[][] res_1 = ((double[][])(new double[][]{}));
        int i_4 = 0;
        while (i_4 < n) {
            double[] row_1 = ((double[])(new double[]{}));
            int j_1 = 0;
            while (j_1 < m) {
                double s_1 = 0.0;
                int k = 0;
                while (k < p) {
                    s_1 = s_1 + a[i_4][k] * b[k][j_1];
                    k = k + 1;
                }
                row_1 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row_1), java.util.stream.DoubleStream.of(s_1)).toArray()));
                j_1 = j_1 + 1;
            }
            res_1 = ((double[][])(appendObj(res_1, row_1)));
            i_4 = i_4 + 1;
        }
        return res_1;
    }

    static double[] matvec(double[][] a, double[] b) {
        double[] res_2 = ((double[])(new double[]{}));
        int i_5 = 0;
        while (i_5 < a.length) {
            res_2 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(res_2), java.util.stream.DoubleStream.of(dot(((double[])(a[i_5])), ((double[])(b))))).toArray()));
            i_5 = i_5 + 1;
        }
        return res_2;
    }

    static double[][] identity(int n) {
        double[][] res_3 = ((double[][])(new double[][]{}));
        int i_6 = 0;
        while (i_6 < n) {
            double[] row_2 = ((double[])(new double[]{}));
            int j_2 = 0;
            while (j_2 < n) {
                row_2 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row_2), java.util.stream.DoubleStream.of(i_6 == j_2 ? 1.0 : 0.0)).toArray()));
                j_2 = j_2 + 1;
            }
            res_3 = ((double[][])(appendObj(res_3, row_2)));
            i_6 = i_6 + 1;
        }
        return res_3;
    }

    static double[][] invert(double[][] mat) {
        int n_1 = mat.length;
        double[][] a = ((double[][])(mat));
        double[][] inv = ((double[][])(identity(n_1)));
        int i_7 = 0;
        while (i_7 < n_1) {
            double pivot = a[i_7][i_7];
            int j_3 = 0;
            while (j_3 < n_1) {
a[i_7][j_3] = a[i_7][j_3] / pivot;
inv[i_7][j_3] = inv[i_7][j_3] / pivot;
                j_3 = j_3 + 1;
            }
            int k_1 = 0;
            while (k_1 < n_1) {
                if (k_1 != i_7) {
                    double factor = a[k_1][i_7];
                    j_3 = 0;
                    while (j_3 < n_1) {
a[k_1][j_3] = a[k_1][j_3] - factor * a[i_7][j_3];
inv[k_1][j_3] = inv[k_1][j_3] - factor * inv[i_7][j_3];
                        j_3 = j_3 + 1;
                    }
                }
                k_1 = k_1 + 1;
            }
            i_7 = i_7 + 1;
        }
        return inv;
    }

    static double[] normal_equation(double[][] X, double[] y) {
        double[][] Xt = ((double[][])(transpose(((double[][])(X)))));
        double[][] XtX = ((double[][])(matmul(((double[][])(Xt)), ((double[][])(X)))));
        double[][] XtX_inv = ((double[][])(invert(((double[][])(XtX)))));
        double[] Xty = ((double[])(matvec(((double[][])(Xt)), ((double[])(y)))));
        return matvec(((double[][])(XtX_inv)), ((double[])(Xty)));
    }

    static double linear_regression_prediction(double[] train_dt, double[] train_usr, double[] train_mtch, double[] test_dt, double[] test_mtch) {
        Object X = new double[][]{};
        int i_8 = 0;
        while (i_8 < train_dt.length) {
            X = appendObj(X, new double[]{1.0, train_dt[i_8], train_mtch[i_8]});
            i_8 = i_8 + 1;
        }
        double[] beta = ((double[])(normal_equation(((double[][])(X)), ((double[])(train_usr)))));
        return abs_float(beta[0] + test_dt[0] * beta[1] + test_mtch[0] * beta[2]);
    }

    static double sarimax_predictor(double[] train_user, double[] train_match, double[] test_match) {
        int n_2 = train_user.length;
        Object X_1 = new double[][]{};
        double[] y = ((double[])(new double[]{}));
        int i_9 = 1;
        while (i_9 < n_2) {
            X_1 = appendObj(X_1, new double[]{1.0, train_user[i_9 - 1], train_match[i_9]});
            y = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(y), java.util.stream.DoubleStream.of(train_user[i_9])).toArray()));
            i_9 = i_9 + 1;
        }
        double[] beta_1 = ((double[])(normal_equation(((double[][])(X_1)), ((double[])(y)))));
        return beta_1[0] + beta_1[1] * train_user[n_2 - 1] + beta_1[2] * test_match[0];
    }

    static double rbf_kernel(double[] a, double[] b, double gamma) {
        double sum_1 = 0.0;
        int i_10 = 0;
        while (i_10 < a.length) {
            double diff = a[i_10] - b[i_10];
            sum_1 = sum_1 + diff * diff;
            i_10 = i_10 + 1;
        }
        return exp_approx(-gamma * sum_1);
    }

    static double support_vector_regressor(double[][] x_train, double[][] x_test, double[] train_user) {
        double gamma = 0.1;
        double[] weights = ((double[])(new double[]{}));
        int i_11 = 0;
        while (i_11 < x_train.length) {
            weights = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(weights), java.util.stream.DoubleStream.of(rbf_kernel(((double[])(x_train[i_11])), ((double[])(x_test[0])), gamma))).toArray()));
            i_11 = i_11 + 1;
        }
        double num = 0.0;
        double den = 0.0;
        i_11 = 0;
        while (i_11 < train_user.length) {
            num = num + weights[i_11] * train_user[i_11];
            den = den + weights[i_11];
            i_11 = i_11 + 1;
        }
        return num / den;
    }

    static double[] set_at_float(double[] xs, int idx, double value) {
        int i_12 = 0;
        double[] res_4 = ((double[])(new double[]{}));
        while (i_12 < xs.length) {
            if (i_12 == idx) {
                res_4 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(res_4), java.util.stream.DoubleStream.of(value)).toArray()));
            } else {
                res_4 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(res_4), java.util.stream.DoubleStream.of(xs[i_12])).toArray()));
            }
            i_12 = i_12 + 1;
        }
        return res_4;
    }

    static double[] sort_float(double[] xs) {
        double[] res_5 = ((double[])(xs));
        int i_13 = 1;
        while (i_13 < res_5.length) {
            double key = res_5[i_13];
            int j_4 = i_13 - 1;
            while (j_4 >= 0 && res_5[j_4] > key) {
                res_5 = ((double[])(set_at_float(((double[])(res_5)), j_4 + 1, res_5[j_4])));
                j_4 = j_4 - 1;
            }
            res_5 = ((double[])(set_at_float(((double[])(res_5)), j_4 + 1, key)));
            i_13 = i_13 + 1;
        }
        return res_5;
    }

    static double percentile(double[] data, double q) {
        double[] sorted = ((double[])(sort_float(((double[])(data)))));
        int n_3 = sorted.length;
        double pos = (q / 100.0) * int_to_float(n_3 - 1);
        int idx = floor_int(pos);
        double frac = pos - int_to_float(idx);
        if (idx + 1 < n_3) {
            return sorted[idx] * (1.0 - frac) + sorted[idx + 1] * frac;
        }
        return sorted[idx];
    }

    static double interquartile_range_checker(double[] train_user) {
        double q1 = percentile(((double[])(train_user)), 25.0);
        double q3 = percentile(((double[])(train_user)), 75.0);
        double iqr = q3 - q1;
        return q1 - iqr * 0.1;
    }

    static boolean data_safety_checker(double[] list_vote, double actual_result) {
        int safe = 0;
        int not_safe = 0;
        int i_14 = 0;
        while (i_14 < list_vote.length) {
            double v = list_vote[i_14];
            if (v > actual_result) {
                safe = not_safe + 1;
            } else             if (abs_float(abs_float(v) - abs_float(actual_result)) <= 0.1) {
                safe = safe + 1;
            } else {
                not_safe = not_safe + 1;
            }
            i_14 = i_14 + 1;
        }
        return safe > not_safe;
    }

    static void main() {
        double[] vote = ((double[])(new double[]{linear_regression_prediction(((double[])(new double[]{2.0, 3.0, 4.0, 5.0})), ((double[])(new double[]{5.0, 3.0, 4.0, 6.0})), ((double[])(new double[]{3.0, 1.0, 2.0, 4.0})), ((double[])(new double[]{2.0})), ((double[])(new double[]{2.0}))), sarimax_predictor(((double[])(new double[]{4.0, 2.0, 6.0, 8.0})), ((double[])(new double[]{3.0, 1.0, 2.0, 4.0})), ((double[])(new double[]{2.0}))), support_vector_regressor(((double[][])(new double[][]{new double[]{5.0, 2.0}, new double[]{1.0, 5.0}, new double[]{6.0, 2.0}})), ((double[][])(new double[][]{new double[]{3.0, 2.0}})), ((double[])(new double[]{2.0, 1.0, 4.0})))}));
        System.out.println(vote[0]);
        System.out.println(vote[1]);
        System.out.println(vote[2]);
        System.out.println(data_safety_checker(((double[])(vote)), 5.0));
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

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }
}
