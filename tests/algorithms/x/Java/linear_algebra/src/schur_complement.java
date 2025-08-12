public class Main {
    static class OptionMatrix {
        double[][] value;
        boolean ok;
        OptionMatrix(double[][] value, boolean ok) {
            this.value = value;
            this.ok = ok;
        }
        OptionMatrix() {}
        @Override public String toString() {
            return String.format("{'value': %s, 'ok': %s}", String.valueOf(value), String.valueOf(ok));
        }
    }


    static double[][] identity(long n) {
        double[][] mat = ((double[][])(new double[][]{}));
        long i_1 = 0L;
        while (i_1 < n) {
            double[] row_1 = ((double[])(new double[]{}));
            long j_1 = 0L;
            while (j_1 < n) {
                if (i_1 == j_1) {
                    row_1 = ((double[])(appendDouble(row_1, 1.0)));
                } else {
                    row_1 = ((double[])(appendDouble(row_1, 0.0)));
                }
                j_1 = j_1 + 1;
            }
            mat = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(mat), java.util.stream.Stream.of(row_1)).toArray(double[][]::new)));
            i_1 = i_1 + 1;
        }
        return mat;
    }

    static double[][] transpose(double[][] mat) {
        long rows = mat.length;
        long cols_1 = mat[(int)((long)(0))].length;
        double[][] res_1 = ((double[][])(new double[][]{}));
        long j_3 = 0L;
        while (j_3 < cols_1) {
            double[] row_3 = ((double[])(new double[]{}));
            long i_3 = 0L;
            while (i_3 < rows) {
                row_3 = ((double[])(appendDouble(row_3, mat[(int)((long)(i_3))][(int)((long)(j_3))])));
                i_3 = i_3 + 1;
            }
            res_1 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(res_1), java.util.stream.Stream.of(row_3)).toArray(double[][]::new)));
            j_3 = j_3 + 1;
        }
        return res_1;
    }

    static double[][] matmul(double[][] a, double[][] b) {
        long rows_1 = a.length;
        long cols_3 = b[(int)((long)(0))].length;
        long inner_1 = a[(int)((long)(0))].length;
        double[][] res_3 = ((double[][])(new double[][]{}));
        long i_5 = 0L;
        while (i_5 < rows_1) {
            double[] row_5 = ((double[])(new double[]{}));
            long j_5 = 0L;
            while (j_5 < cols_3) {
                double sum_1 = 0.0;
                long k_1 = 0L;
                while (k_1 < inner_1) {
                    sum_1 = sum_1 + a[(int)((long)(i_5))][(int)((long)(k_1))] * b[(int)((long)(k_1))][(int)((long)(j_5))];
                    k_1 = k_1 + 1;
                }
                row_5 = ((double[])(appendDouble(row_5, sum_1)));
                j_5 = j_5 + 1;
            }
            res_3 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(res_3), java.util.stream.Stream.of(row_5)).toArray(double[][]::new)));
            i_5 = i_5 + 1;
        }
        return res_3;
    }

    static double[][] mat_sub(double[][] a, double[][] b) {
        long rows_2 = a.length;
        long cols_5 = a[(int)((long)(0))].length;
        double[][] res_5 = ((double[][])(new double[][]{}));
        long i_7 = 0L;
        while (i_7 < rows_2) {
            double[] row_7 = ((double[])(new double[]{}));
            long j_7 = 0L;
            while (j_7 < cols_5) {
                row_7 = ((double[])(appendDouble(row_7, a[(int)((long)(i_7))][(int)((long)(j_7))] - b[(int)((long)(i_7))][(int)((long)(j_7))])));
                j_7 = j_7 + 1;
            }
            res_5 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(res_5), java.util.stream.Stream.of(row_7)).toArray(double[][]::new)));
            i_7 = i_7 + 1;
        }
        return res_5;
    }

    static double[][] inverse(double[][] mat) {
        long n = mat.length;
        double[][] id_1 = ((double[][])(identity(n)));
        double[][] aug_1 = ((double[][])(new double[][]{}));
        long i_9 = 0L;
        while (i_9 < n) {
            Object row_9 = concat(mat[(int)((long)(i_9))], id_1[(int)((long)(i_9))]);
            aug_1 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(aug_1), java.util.stream.Stream.of(row_9)).toArray(double[][]::new)));
            i_9 = i_9 + 1;
        }
        long col_1 = 0L;
        while (col_1 < n) {
            double[] pivot_row_1 = ((double[])(aug_1[(int)((long)(col_1))]));
            double pivot_1 = pivot_row_1[(int)((long)(col_1))];
            if (pivot_1 == 0.0) {
                throw new RuntimeException(String.valueOf("matrix is singular"));
            }
            long j_9 = 0L;
            while (j_9 < 2 * n) {
pivot_row_1[(int)((long)(j_9))] = pivot_row_1[(int)((long)(j_9))] / pivot_1;
                j_9 = j_9 + 1;
            }
aug_1[(int)((long)(col_1))] = ((double[])(pivot_row_1));
            long r_1 = 0L;
            while (r_1 < n) {
                if (r_1 != col_1) {
                    double[] row_r_1 = ((double[])(aug_1[(int)((long)(r_1))]));
                    double factor_1 = row_r_1[(int)((long)(col_1))];
                    j_9 = 0;
                    while (j_9 < 2 * n) {
row_r_1[(int)((long)(j_9))] = row_r_1[(int)((long)(j_9))] - factor_1 * pivot_row_1[(int)((long)(j_9))];
                        j_9 = j_9 + 1;
                    }
aug_1[(int)((long)(r_1))] = ((double[])(row_r_1));
                }
                r_1 = r_1 + 1;
            }
            col_1 = col_1 + 1;
        }
        double[][] inv_1 = ((double[][])(new double[][]{}));
        long r_3 = 0L;
        while (r_3 < n) {
            double[] row_11 = ((double[])(new double[]{}));
            long c_1 = n;
            while (c_1 < 2 * n) {
                row_11 = ((double[])(appendDouble(row_11, aug_1[(int)((long)(r_3))][(int)((long)(c_1))])));
                c_1 = c_1 + 1;
            }
            inv_1 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(inv_1), java.util.stream.Stream.of(row_11)).toArray(double[][]::new)));
            r_3 = r_3 + 1;
        }
        return inv_1;
    }

    static double[][] schur_complement(double[][] mat_a, double[][] mat_b, double[][] mat_c, OptionMatrix pseudo_inv) {
        long a_rows = mat_a.length;
        long a_cols_1 = mat_a[(int)((long)(0))].length;
        if (a_rows != a_cols_1) {
            throw new RuntimeException(String.valueOf("Matrix A must be square"));
        }
        if (a_rows != mat_b.length) {
            throw new RuntimeException(String.valueOf("Expected the same number of rows for A and B"));
        }
        if (mat_b[(int)((long)(0))].length != mat_c[(int)((long)(0))].length) {
            throw new RuntimeException(String.valueOf("Expected the same number of columns for B and C"));
        }
        double[][] a_inv_1 = new double[0][];
        if (pseudo_inv.ok) {
            a_inv_1 = ((double[][])(pseudo_inv.value));
        } else {
            a_inv_1 = ((double[][])(inverse(((double[][])(mat_a)))));
        }
        double[][] bt_1 = ((double[][])(transpose(((double[][])(mat_b)))));
        double[][] a_inv_b_1 = ((double[][])(matmul(((double[][])(a_inv_1)), ((double[][])(mat_b)))));
        double[][] bt_a_inv_b_1 = ((double[][])(matmul(((double[][])(bt_1)), ((double[][])(a_inv_b_1)))));
        return mat_sub(((double[][])(mat_c)), ((double[][])(bt_a_inv_b_1)));
    }

    static void print_matrix(double[][] mat) {
        long i_10 = 0L;
        while (i_10 < mat.length) {
            String line_1 = "";
            long j_11 = 0L;
            double[] row_13 = ((double[])(mat[(int)((long)(i_10))]));
            while (j_11 < row_13.length) {
                line_1 = line_1 + _p(_getd(row_13, ((Number)(j_11)).intValue()));
                if (j_11 + 1 < row_13.length) {
                    line_1 = line_1 + " ";
                }
                j_11 = j_11 + 1;
            }
            System.out.println(line_1);
            i_10 = i_10 + 1;
        }
    }

    static void main() {
        double[][] a = ((double[][])(new double[][]{new double[]{1.0, 2.0}, new double[]{2.0, 1.0}}));
        double[][] b_1 = ((double[][])(new double[][]{new double[]{0.0, 3.0}, new double[]{3.0, 0.0}}));
        double[][] c_3 = ((double[][])(new double[][]{new double[]{2.0, 1.0}, new double[]{6.0, 3.0}}));
        OptionMatrix none_1 = new OptionMatrix(new double[][]{}, false);
        double[][] s_1 = ((double[][])(schur_complement(((double[][])(a)), ((double[][])(b_1)), ((double[][])(c_3)), none_1)));
        print_matrix(((double[][])(s_1)));
    }
    public static void main(String[] args) {
        main();
    }

    static double[] appendDouble(double[] arr, double v) {
        double[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }

    static <T> T[] concat(T[] a, T[] b) {
        T[] out = java.util.Arrays.copyOf(a, a.length + b.length);
        System.arraycopy(b, 0, out, a.length, b.length);
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
            if (d == Math.rint(d)) return String.valueOf((long) d);
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }

    static Double _getd(double[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
