public class Main {
    static double[][] x_train;
    static double[] y_train;
    static double[] preds_1;

    static double expApprox(double x) {
        if (x < 0.0) {
            return 1.0 / expApprox(-x);
        }
        if (x > 1.0) {
            double half = expApprox(x / 2.0);
            return half * half;
        }
        double sum = 1.0;
        double term = 1.0;
        int n = 1;
        while (n < 20) {
            term = term * x / (((Number)(n)).doubleValue());
            sum = sum + term;
            n = n + 1;
        }
        return sum;
    }

    static double[][] transpose(double[][] mat) {
        int rows = mat.length;
        int cols = mat[0].length;
        double[][] res = ((double[][])(new double[][]{}));
        int i = 0;
        while (i < cols) {
            double[] row = ((double[])(new double[]{}));
            int j = 0;
            while (j < rows) {
                row = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row), java.util.stream.DoubleStream.of(mat[j][i])).toArray()));
                j = j + 1;
            }
            res = ((double[][])(appendObj(res, row)));
            i = i + 1;
        }
        return res;
    }

    static double[][] matMul(double[][] a, double[][] b) {
        int a_rows = a.length;
        int a_cols = a[0].length;
        int b_cols = b[0].length;
        double[][] res_1 = ((double[][])(new double[][]{}));
        int i_1 = 0;
        while (i_1 < a_rows) {
            double[] row_1 = ((double[])(new double[]{}));
            int j_1 = 0;
            while (j_1 < b_cols) {
                double sum_1 = 0.0;
                int k = 0;
                while (k < a_cols) {
                    sum_1 = sum_1 + a[i_1][k] * b[k][j_1];
                    k = k + 1;
                }
                row_1 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row_1), java.util.stream.DoubleStream.of(sum_1)).toArray()));
                j_1 = j_1 + 1;
            }
            res_1 = ((double[][])(appendObj(res_1, row_1)));
            i_1 = i_1 + 1;
        }
        return res_1;
    }

    static double[][] matInv(double[][] mat) {
        int n_1 = mat.length;
        double[][] aug = ((double[][])(new double[][]{}));
        int i_2 = 0;
        while (i_2 < n_1) {
            double[] row_2 = ((double[])(new double[]{}));
            int j_2 = 0;
            while (j_2 < n_1) {
                row_2 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row_2), java.util.stream.DoubleStream.of(mat[i_2][j_2])).toArray()));
                j_2 = j_2 + 1;
            }
            j_2 = 0;
            while (j_2 < n_1) {
                if (i_2 == j_2) {
                    row_2 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row_2), java.util.stream.DoubleStream.of(1.0)).toArray()));
                } else {
                    row_2 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row_2), java.util.stream.DoubleStream.of(0.0)).toArray()));
                }
                j_2 = j_2 + 1;
            }
            aug = ((double[][])(appendObj(aug, row_2)));
            i_2 = i_2 + 1;
        }
        int col = 0;
        while (col < n_1) {
            double pivot = aug[col][col];
            if (pivot == 0.0) {
                throw new RuntimeException(String.valueOf("Matrix is singular"));
            }
            int j_3 = 0;
            while (j_3 < 2 * n_1) {
aug[col][j_3] = aug[col][j_3] / pivot;
                j_3 = j_3 + 1;
            }
            int r = 0;
            while (r < n_1) {
                if (r != col) {
                    double factor = aug[r][col];
                    j_3 = 0;
                    while (j_3 < 2 * n_1) {
aug[r][j_3] = aug[r][j_3] - factor * aug[col][j_3];
                        j_3 = j_3 + 1;
                    }
                }
                r = r + 1;
            }
            col = col + 1;
        }
        double[][] inv = ((double[][])(new double[][]{}));
        i_2 = 0;
        while (i_2 < n_1) {
            double[] row_3 = ((double[])(new double[]{}));
            int j_4 = 0;
            while (j_4 < n_1) {
                row_3 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row_3), java.util.stream.DoubleStream.of(aug[i_2][j_4 + n_1])).toArray()));
                j_4 = j_4 + 1;
            }
            inv = ((double[][])(appendObj(inv, row_3)));
            i_2 = i_2 + 1;
        }
        return inv;
    }

    static double[][] weight_matrix(double[] point, double[][] x_train, double tau) {
        int m = x_train.length;
        double[][] weights = ((double[][])(new double[][]{}));
        int i_3 = 0;
        while (i_3 < m) {
            double[] row_4 = ((double[])(new double[]{}));
            int j_5 = 0;
            while (j_5 < m) {
                if (i_3 == j_5) {
                    row_4 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row_4), java.util.stream.DoubleStream.of(1.0)).toArray()));
                } else {
                    row_4 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row_4), java.util.stream.DoubleStream.of(0.0)).toArray()));
                }
                j_5 = j_5 + 1;
            }
            weights = ((double[][])(appendObj(weights, row_4)));
            i_3 = i_3 + 1;
        }
        int j_6 = 0;
        while (j_6 < m) {
            double diff_sq = 0.0;
            int k_1 = 0;
            while (k_1 < point.length) {
                double diff = point[k_1] - x_train[j_6][k_1];
                diff_sq = diff_sq + diff * diff;
                k_1 = k_1 + 1;
            }
weights[j_6][j_6] = expApprox(-diff_sq / (2.0 * tau * tau));
            j_6 = j_6 + 1;
        }
        return weights;
    }

    static double[][] local_weight(double[] point, double[][] x_train, double[] y_train, double tau) {
        double[][] w = ((double[][])(weight_matrix(((double[])(point)), ((double[][])(x_train)), tau)));
        double[][] x_t = ((double[][])(transpose(((double[][])(x_train)))));
        double[][] x_t_w = ((double[][])(matMul(((double[][])(x_t)), ((double[][])(w)))));
        double[][] x_t_w_x = ((double[][])(matMul(((double[][])(x_t_w)), ((double[][])(x_train)))));
        double[][] inv_part = ((double[][])(matInv(((double[][])(x_t_w_x)))));
        double[][] y_col = ((double[][])(new double[][]{}));
        int i_4 = 0;
        while (i_4 < y_train.length) {
            y_col = ((double[][])(appendObj(y_col, new double[]{y_train[i_4]})));
            i_4 = i_4 + 1;
        }
        double[][] x_t_w_y = ((double[][])(matMul(((double[][])(x_t_w)), ((double[][])(y_col)))));
        return matMul(((double[][])(inv_part)), ((double[][])(x_t_w_y)));
    }

    static double[] local_weight_regression(double[][] x_train, double[] y_train, double tau) {
        int m_1 = x_train.length;
        double[] preds = ((double[])(new double[]{}));
        int i_5 = 0;
        while (i_5 < m_1) {
            double[][] theta = ((double[][])(local_weight(((double[])(x_train[i_5])), ((double[][])(x_train)), ((double[])(y_train)), tau)));
            double[] weights_vec = ((double[])(new double[]{}));
            int k_2 = 0;
            while (k_2 < theta.length) {
                weights_vec = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(weights_vec), java.util.stream.DoubleStream.of(theta[k_2][0])).toArray()));
                k_2 = k_2 + 1;
            }
            double pred = 0.0;
            int j_7 = 0;
            while (j_7 < x_train[i_5].length) {
                pred = pred + x_train[i_5][j_7] * weights_vec[j_7];
                j_7 = j_7 + 1;
            }
            preds = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(preds), java.util.stream.DoubleStream.of(pred)).toArray()));
            i_5 = i_5 + 1;
        }
        return preds;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            x_train = ((double[][])(new double[][]{new double[]{16.99, 10.34}, new double[]{21.01, 23.68}, new double[]{24.59, 25.69}}));
            y_train = ((double[])(new double[]{1.01, 1.66, 3.5}));
            preds_1 = ((double[])(local_weight_regression(((double[][])(x_train)), ((double[])(y_train)), 0.6)));
            json(preds_1);
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
