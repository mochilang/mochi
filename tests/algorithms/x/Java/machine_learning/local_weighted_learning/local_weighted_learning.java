public class Main {
    static double[][] x_train;
    static double[] y_train;
    static double[] preds_2;

    static double expApprox(double x) {
        if (x < 0.0) {
            return 1.0 / expApprox(-x);
        }
        if (x > 1.0) {
            double half_1 = expApprox(x / 2.0);
            return half_1 * half_1;
        }
        double sum_1 = 1.0;
        double term_1 = 1.0;
        long n_1 = 1;
        while (n_1 < 20) {
            term_1 = term_1 * x / (((Number)(n_1)).doubleValue());
            sum_1 = sum_1 + term_1;
            n_1 = n_1 + 1;
        }
        return sum_1;
    }

    static double[][] transpose(double[][] mat) {
        long rows = mat.length;
        long cols_1 = mat[(int)(0)].length;
        double[][] res_1 = ((double[][])(new double[][]{}));
        long i_1 = 0;
        while (i_1 < cols_1) {
            double[] row_1 = ((double[])(new double[]{}));
            long j_1 = 0;
            while (j_1 < rows) {
                row_1 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row_1), java.util.stream.DoubleStream.of(mat[(int)(j_1)][(int)(i_1)])).toArray()));
                j_1 = j_1 + 1;
            }
            res_1 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(res_1), java.util.stream.Stream.of(row_1)).toArray(double[][]::new)));
            i_1 = i_1 + 1;
        }
        return res_1;
    }

    static double[][] matMul(double[][] a, double[][] b) {
        long a_rows = a.length;
        long a_cols_1 = a[(int)(0)].length;
        long b_cols_1 = b[(int)(0)].length;
        double[][] res_3 = ((double[][])(new double[][]{}));
        long i_3 = 0;
        while (i_3 < a_rows) {
            double[] row_3 = ((double[])(new double[]{}));
            long j_3 = 0;
            while (j_3 < b_cols_1) {
                double sum_3 = 0.0;
                long k_1 = 0;
                while (k_1 < a_cols_1) {
                    sum_3 = sum_3 + a[(int)(i_3)][(int)(k_1)] * b[(int)(k_1)][(int)(j_3)];
                    k_1 = k_1 + 1;
                }
                row_3 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row_3), java.util.stream.DoubleStream.of(sum_3)).toArray()));
                j_3 = j_3 + 1;
            }
            res_3 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(res_3), java.util.stream.Stream.of(row_3)).toArray(double[][]::new)));
            i_3 = i_3 + 1;
        }
        return res_3;
    }

    static double[][] matInv(double[][] mat) {
        long n_2 = mat.length;
        double[][] aug_1 = ((double[][])(new double[][]{}));
        long i_5 = 0;
        while (i_5 < n_2) {
            double[] row_5 = ((double[])(new double[]{}));
            long j_5 = 0;
            while (j_5 < n_2) {
                row_5 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row_5), java.util.stream.DoubleStream.of(mat[(int)(i_5)][(int)(j_5)])).toArray()));
                j_5 = j_5 + 1;
            }
            j_5 = 0;
            while (j_5 < n_2) {
                if (i_5 == j_5) {
                    row_5 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row_5), java.util.stream.DoubleStream.of(1.0)).toArray()));
                } else {
                    row_5 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row_5), java.util.stream.DoubleStream.of(0.0)).toArray()));
                }
                j_5 = j_5 + 1;
            }
            aug_1 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(aug_1), java.util.stream.Stream.of(row_5)).toArray(double[][]::new)));
            i_5 = i_5 + 1;
        }
        long col_1 = 0;
        while (col_1 < n_2) {
            double pivot_1 = aug_1[(int)(col_1)][(int)(col_1)];
            if (pivot_1 == 0.0) {
                throw new RuntimeException(String.valueOf("Matrix is singular"));
            }
            long j_7 = 0;
            while (j_7 < 2 * n_2) {
aug_1[(int)(col_1)][(int)(j_7)] = aug_1[(int)(col_1)][(int)(j_7)] / pivot_1;
                j_7 = j_7 + 1;
            }
            long r_1 = 0;
            while (r_1 < n_2) {
                if (r_1 != col_1) {
                    double factor_1 = aug_1[(int)(r_1)][(int)(col_1)];
                    j_7 = 0;
                    while (j_7 < 2 * n_2) {
aug_1[(int)(r_1)][(int)(j_7)] = aug_1[(int)(r_1)][(int)(j_7)] - factor_1 * aug_1[(int)(col_1)][(int)(j_7)];
                        j_7 = j_7 + 1;
                    }
                }
                r_1 = r_1 + 1;
            }
            col_1 = col_1 + 1;
        }
        double[][] inv_1 = ((double[][])(new double[][]{}));
        i_5 = 0;
        while (i_5 < n_2) {
            double[] row_7 = ((double[])(new double[]{}));
            long j_9 = 0;
            while (j_9 < n_2) {
                row_7 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row_7), java.util.stream.DoubleStream.of(aug_1[(int)(i_5)][(int)(j_9 + n_2)])).toArray()));
                j_9 = j_9 + 1;
            }
            inv_1 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(inv_1), java.util.stream.Stream.of(row_7)).toArray(double[][]::new)));
            i_5 = i_5 + 1;
        }
        return inv_1;
    }

    static double[][] weight_matrix(double[] point, double[][] x_train, double tau) {
        long m = x_train.length;
        double[][] weights_1 = ((double[][])(new double[][]{}));
        long i_7 = 0;
        while (i_7 < m) {
            double[] row_9 = ((double[])(new double[]{}));
            long j_11 = 0;
            while (j_11 < m) {
                if (i_7 == j_11) {
                    row_9 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row_9), java.util.stream.DoubleStream.of(1.0)).toArray()));
                } else {
                    row_9 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row_9), java.util.stream.DoubleStream.of(0.0)).toArray()));
                }
                j_11 = j_11 + 1;
            }
            weights_1 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(weights_1), java.util.stream.Stream.of(row_9)).toArray(double[][]::new)));
            i_7 = i_7 + 1;
        }
        long j_13 = 0;
        while (j_13 < m) {
            double diff_sq_1 = 0.0;
            long k_3 = 0;
            while (k_3 < point.length) {
                double diff_1 = point[(int)(k_3)] - x_train[(int)(j_13)][(int)(k_3)];
                diff_sq_1 = diff_sq_1 + diff_1 * diff_1;
                k_3 = k_3 + 1;
            }
weights_1[(int)(j_13)][(int)(j_13)] = expApprox(-diff_sq_1 / (2.0 * tau * tau));
            j_13 = j_13 + 1;
        }
        return weights_1;
    }

    static double[][] local_weight(double[] point, double[][] x_train, double[] y_train, double tau) {
        double[][] w = ((double[][])(weight_matrix(((double[])(point)), ((double[][])(x_train)), tau)));
        double[][] x_t_1 = ((double[][])(transpose(((double[][])(x_train)))));
        double[][] x_t_w_1 = ((double[][])(matMul(((double[][])(x_t_1)), ((double[][])(w)))));
        double[][] x_t_w_x_1 = ((double[][])(matMul(((double[][])(x_t_w_1)), ((double[][])(x_train)))));
        double[][] inv_part_1 = ((double[][])(matInv(((double[][])(x_t_w_x_1)))));
        double[][] y_col_1 = ((double[][])(new double[][]{}));
        long i_9 = 0;
        while (i_9 < y_train.length) {
            y_col_1 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(y_col_1), java.util.stream.Stream.of(new double[]{y_train[(int)(i_9)]})).toArray(double[][]::new)));
            i_9 = i_9 + 1;
        }
        double[][] x_t_w_y_1 = ((double[][])(matMul(((double[][])(x_t_w_1)), ((double[][])(y_col_1)))));
        return matMul(((double[][])(inv_part_1)), ((double[][])(x_t_w_y_1)));
    }

    static double[] local_weight_regression(double[][] x_train, double[] y_train, double tau) {
        long m_1 = x_train.length;
        double[] preds_1 = ((double[])(new double[]{}));
        long i_11 = 0;
        while (i_11 < m_1) {
            double[][] theta_1 = ((double[][])(local_weight(((double[])(x_train[(int)(i_11)])), ((double[][])(x_train)), ((double[])(y_train)), tau)));
            double[] weights_vec_1 = ((double[])(new double[]{}));
            long k_5 = 0;
            while (k_5 < theta_1.length) {
                weights_vec_1 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(weights_vec_1), java.util.stream.DoubleStream.of(theta_1[(int)(k_5)][(int)(0)])).toArray()));
                k_5 = k_5 + 1;
            }
            double pred_1 = 0.0;
            long j_15 = 0;
            while (j_15 < x_train[(int)(i_11)].length) {
                pred_1 = pred_1 + x_train[(int)(i_11)][(int)(j_15)] * weights_vec_1[(int)(j_15)];
                j_15 = j_15 + 1;
            }
            preds_1 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(preds_1), java.util.stream.DoubleStream.of(pred_1)).toArray()));
            i_11 = i_11 + 1;
        }
        return preds_1;
    }
    public static void main(String[] args) {
        x_train = ((double[][])(new double[][]{new double[]{16.99, 10.34}, new double[]{21.01, 23.68}, new double[]{24.59, 25.69}}));
        y_train = ((double[])(new double[]{1.01, 1.66, 3.5}));
        preds_2 = ((double[])(local_weight_regression(((double[][])(x_train)), ((double[])(y_train)), 0.6)));
        json(preds_2);
    }

    static void json(Object v) {
        System.out.println(_json(v));
    }

    static String _json(Object v) {
        if (v == null) return "null";
        if (v instanceof String) {
            String s = (String)v;
            s = s.replace("\\", "\\\\").replace("\"", "\\\"");
            return "\"" + s + "\"";
        }
        if (v instanceof Number || v instanceof Boolean) {
            return String.valueOf(v);
        }
        if (v instanceof int[]) {
            int[] a = (int[]) v;
            StringBuilder sb = new StringBuilder();
            sb.append("[");
            for (int i = 0; i < a.length; i++) { if (i > 0) sb.append(","); sb.append(a[i]); }
            sb.append("]");
            return sb.toString();
        }
        if (v instanceof double[]) {
            double[] a = (double[]) v;
            StringBuilder sb = new StringBuilder();
            sb.append("[");
            for (int i = 0; i < a.length; i++) { if (i > 0) sb.append(","); sb.append(a[i]); }
            sb.append("]");
            return sb.toString();
        }
        if (v instanceof boolean[]) {
            boolean[] a = (boolean[]) v;
            StringBuilder sb = new StringBuilder();
            sb.append("[");
            for (int i = 0; i < a.length; i++) { if (i > 0) sb.append(","); sb.append(a[i]); }
            sb.append("]");
            return sb.toString();
        }
        if (v.getClass().isArray()) {
            Object[] a = (Object[]) v;
            StringBuilder sb = new StringBuilder();
            sb.append("[");
            for (int i = 0; i < a.length; i++) { if (i > 0) sb.append(","); sb.append(_json(a[i])); }
            sb.append("]");
            return sb.toString();
        }
        String s = String.valueOf(v);
        s = s.replace("\\", "\\\\").replace("\"", "\\\"");
        return "\"" + s + "\"";
    }
}
