public class Main {
    static double[][] x_train = ((double[][])(new double[][]{new double[]{16.99, 10.34}, new double[]{21.01, 23.68}, new double[]{24.59, 25.69}}));
    static double[] y_train = ((double[])(new double[]{1.01, 1.66, 3.5}));
    static double[] preds_2;

    static double expApprox(double x) {
        if ((double)(x) < (double)(0.0)) {
            return (double)(1.0) / (double)(expApprox((double)(-x)));
        }
        if ((double)(x) > (double)(1.0)) {
            double half_1 = (double)(expApprox((double)((double)(x) / (double)(2.0))));
            return (double)(half_1) * (double)(half_1);
        }
        double sum_1 = (double)(1.0);
        double term_1 = (double)(1.0);
        long n_1 = 1L;
        while ((long)(n_1) < 20L) {
            term_1 = (double)((double)((double)(term_1) * (double)(x)) / (double)((((Number)(n_1)).doubleValue())));
            sum_1 = (double)((double)(sum_1) + (double)(term_1));
            n_1 = (long)((long)(n_1) + 1L);
        }
        return sum_1;
    }

    static double[][] transpose(double[][] mat) {
        long rows = (long)(mat.length);
        long cols_1 = (long)(mat[(int)(0L)].length);
        double[][] res_1 = ((double[][])(new double[][]{}));
        long i_1 = 0L;
        while ((long)(i_1) < (long)(cols_1)) {
            double[] row_1 = ((double[])(new double[]{}));
            long j_1 = 0L;
            while ((long)(j_1) < (long)(rows)) {
                row_1 = ((double[])(appendDouble(row_1, (double)(mat[(int)((long)(j_1))][(int)((long)(i_1))]))));
                j_1 = (long)((long)(j_1) + 1L);
            }
            res_1 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(res_1), java.util.stream.Stream.of(new double[][]{row_1})).toArray(double[][]::new)));
            i_1 = (long)((long)(i_1) + 1L);
        }
        return res_1;
    }

    static double[][] matMul(double[][] a, double[][] b) {
        long a_rows = (long)(a.length);
        long a_cols_1 = (long)(a[(int)(0L)].length);
        long b_cols_1 = (long)(b[(int)(0L)].length);
        double[][] res_3 = ((double[][])(new double[][]{}));
        long i_3 = 0L;
        while ((long)(i_3) < (long)(a_rows)) {
            double[] row_3 = ((double[])(new double[]{}));
            long j_3 = 0L;
            while ((long)(j_3) < (long)(b_cols_1)) {
                double sum_3 = (double)(0.0);
                long k_1 = 0L;
                while ((long)(k_1) < (long)(a_cols_1)) {
                    sum_3 = (double)((double)(sum_3) + (double)((double)(a[(int)((long)(i_3))][(int)((long)(k_1))]) * (double)(b[(int)((long)(k_1))][(int)((long)(j_3))])));
                    k_1 = (long)((long)(k_1) + 1L);
                }
                row_3 = ((double[])(appendDouble(row_3, (double)(sum_3))));
                j_3 = (long)((long)(j_3) + 1L);
            }
            res_3 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(res_3), java.util.stream.Stream.of(new double[][]{row_3})).toArray(double[][]::new)));
            i_3 = (long)((long)(i_3) + 1L);
        }
        return res_3;
    }

    static double[][] matInv(double[][] mat) {
        long n_2 = (long)(mat.length);
        double[][] aug_1 = ((double[][])(new double[][]{}));
        long i_5 = 0L;
        while ((long)(i_5) < (long)(n_2)) {
            double[] row_5 = ((double[])(new double[]{}));
            long j_5 = 0L;
            while ((long)(j_5) < (long)(n_2)) {
                row_5 = ((double[])(appendDouble(row_5, (double)(mat[(int)((long)(i_5))][(int)((long)(j_5))]))));
                j_5 = (long)((long)(j_5) + 1L);
            }
            j_5 = 0L;
            while ((long)(j_5) < (long)(n_2)) {
                if ((long)(i_5) == (long)(j_5)) {
                    row_5 = ((double[])(appendDouble(row_5, (double)(1.0))));
                } else {
                    row_5 = ((double[])(appendDouble(row_5, (double)(0.0))));
                }
                j_5 = (long)((long)(j_5) + 1L);
            }
            aug_1 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(aug_1), java.util.stream.Stream.of(new double[][]{row_5})).toArray(double[][]::new)));
            i_5 = (long)((long)(i_5) + 1L);
        }
        long col_1 = 0L;
        while ((long)(col_1) < (long)(n_2)) {
            double pivot_1 = (double)(aug_1[(int)((long)(col_1))][(int)((long)(col_1))]);
            if ((double)(pivot_1) == (double)(0.0)) {
                throw new RuntimeException(String.valueOf("Matrix is singular"));
            }
            long j_7 = 0L;
            while ((long)(j_7) < (long)(2L * (long)(n_2))) {
aug_1[(int)((long)(col_1))][(int)((long)(j_7))] = (double)((double)(aug_1[(int)((long)(col_1))][(int)((long)(j_7))]) / (double)(pivot_1));
                j_7 = (long)((long)(j_7) + 1L);
            }
            long r_1 = 0L;
            while ((long)(r_1) < (long)(n_2)) {
                if ((long)(r_1) != (long)(col_1)) {
                    double factor_1 = (double)(aug_1[(int)((long)(r_1))][(int)((long)(col_1))]);
                    j_7 = 0L;
                    while ((long)(j_7) < (long)(2L * (long)(n_2))) {
aug_1[(int)((long)(r_1))][(int)((long)(j_7))] = (double)((double)(aug_1[(int)((long)(r_1))][(int)((long)(j_7))]) - (double)((double)(factor_1) * (double)(aug_1[(int)((long)(col_1))][(int)((long)(j_7))])));
                        j_7 = (long)((long)(j_7) + 1L);
                    }
                }
                r_1 = (long)((long)(r_1) + 1L);
            }
            col_1 = (long)((long)(col_1) + 1L);
        }
        double[][] inv_1 = ((double[][])(new double[][]{}));
        i_5 = 0L;
        while ((long)(i_5) < (long)(n_2)) {
            double[] row_7 = ((double[])(new double[]{}));
            long j_9 = 0L;
            while ((long)(j_9) < (long)(n_2)) {
                row_7 = ((double[])(appendDouble(row_7, (double)(aug_1[(int)((long)(i_5))][(int)((long)((long)(j_9) + (long)(n_2)))]))));
                j_9 = (long)((long)(j_9) + 1L);
            }
            inv_1 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(inv_1), java.util.stream.Stream.of(new double[][]{row_7})).toArray(double[][]::new)));
            i_5 = (long)((long)(i_5) + 1L);
        }
        return inv_1;
    }

    static double[][] weight_matrix(double[] point, double[][] x_train, double tau) {
        long m = (long)(x_train.length);
        double[][] weights_1 = ((double[][])(new double[][]{}));
        long i_7 = 0L;
        while ((long)(i_7) < (long)(m)) {
            double[] row_9 = ((double[])(new double[]{}));
            long j_11 = 0L;
            while ((long)(j_11) < (long)(m)) {
                if ((long)(i_7) == (long)(j_11)) {
                    row_9 = ((double[])(appendDouble(row_9, (double)(1.0))));
                } else {
                    row_9 = ((double[])(appendDouble(row_9, (double)(0.0))));
                }
                j_11 = (long)((long)(j_11) + 1L);
            }
            weights_1 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(weights_1), java.util.stream.Stream.of(new double[][]{row_9})).toArray(double[][]::new)));
            i_7 = (long)((long)(i_7) + 1L);
        }
        long j_13 = 0L;
        while ((long)(j_13) < (long)(m)) {
            double diff_sq_1 = (double)(0.0);
            long k_3 = 0L;
            while ((long)(k_3) < (long)(point.length)) {
                double diff_1 = (double)((double)(point[(int)((long)(k_3))]) - (double)(x_train[(int)((long)(j_13))][(int)((long)(k_3))]));
                diff_sq_1 = (double)((double)(diff_sq_1) + (double)((double)(diff_1) * (double)(diff_1)));
                k_3 = (long)((long)(k_3) + 1L);
            }
weights_1[(int)((long)(j_13))][(int)((long)(j_13))] = (double)(expApprox((double)((double)(-diff_sq_1) / (double)(((double)((double)(2.0) * (double)(tau)) * (double)(tau))))));
            j_13 = (long)((long)(j_13) + 1L);
        }
        return weights_1;
    }

    static double[][] local_weight(double[] point, double[][] x_train, double[] y_train, double tau) {
        double[][] w = ((double[][])(weight_matrix(((double[])(point)), ((double[][])(x_train)), (double)(tau))));
        double[][] x_t_1 = ((double[][])(transpose(((double[][])(x_train)))));
        double[][] x_t_w_1 = ((double[][])(matMul(((double[][])(x_t_1)), ((double[][])(w)))));
        double[][] x_t_w_x_1 = ((double[][])(matMul(((double[][])(x_t_w_1)), ((double[][])(x_train)))));
        double[][] inv_part_1 = ((double[][])(matInv(((double[][])(x_t_w_x_1)))));
        double[][] y_col_1 = ((double[][])(new double[][]{}));
        long i_9 = 0L;
        while ((long)(i_9) < (long)(y_train.length)) {
            y_col_1 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(y_col_1), java.util.stream.Stream.of(new double[][]{new double[]{y_train[(int)((long)(i_9))]}})).toArray(double[][]::new)));
            i_9 = (long)((long)(i_9) + 1L);
        }
        double[][] x_t_w_y_1 = ((double[][])(matMul(((double[][])(x_t_w_1)), ((double[][])(y_col_1)))));
        return matMul(((double[][])(inv_part_1)), ((double[][])(x_t_w_y_1)));
    }

    static double[] local_weight_regression(double[][] x_train, double[] y_train, double tau) {
        long m_1 = (long)(x_train.length);
        double[] preds_1 = ((double[])(new double[]{}));
        long i_11 = 0L;
        while ((long)(i_11) < (long)(m_1)) {
            double[][] theta_1 = ((double[][])(local_weight(((double[])(x_train[(int)((long)(i_11))])), ((double[][])(x_train)), ((double[])(y_train)), (double)(tau))));
            double[] weights_vec_1 = ((double[])(new double[]{}));
            long k_5 = 0L;
            while ((long)(k_5) < (long)(theta_1.length)) {
                weights_vec_1 = ((double[])(appendDouble(weights_vec_1, (double)(theta_1[(int)((long)(k_5))][(int)(0L)]))));
                k_5 = (long)((long)(k_5) + 1L);
            }
            double pred_1 = (double)(0.0);
            long j_15 = 0L;
            while ((long)(j_15) < (long)(x_train[(int)((long)(i_11))].length)) {
                pred_1 = (double)((double)(pred_1) + (double)((double)(x_train[(int)((long)(i_11))][(int)((long)(j_15))]) * (double)(weights_vec_1[(int)((long)(j_15))])));
                j_15 = (long)((long)(j_15) + 1L);
            }
            preds_1 = ((double[])(appendDouble(preds_1, (double)(pred_1))));
            i_11 = (long)((long)(i_11) + 1L);
        }
        return preds_1;
    }
    public static void main(String[] args) {
        preds_2 = ((double[])(local_weight_regression(((double[][])(x_train)), ((double[])(y_train)), (double)(0.6))));
        json(preds_2);
    }

    static double[] appendDouble(double[] arr, double v) {
        double[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
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
