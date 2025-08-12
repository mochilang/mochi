public class Main {
    static double[][] mat = new double[0][];

    static double[][] invert_matrix(double[][] matrix) {
        long n = matrix.length;
        double[][] aug_1 = ((double[][])(new double[][]{}));
        long i_1 = 0L;
        while (i_1 < n) {
            double[] row_1 = ((double[])(new double[]{}));
            long j_1 = 0L;
            while (j_1 < n) {
                row_1 = ((double[])(appendDouble(row_1, matrix[(int)((long)(i_1))][(int)((long)(j_1))])));
                j_1 = j_1 + 1;
            }
            long k_1 = 0L;
            while (k_1 < n) {
                if (i_1 == k_1) {
                    row_1 = ((double[])(appendDouble(row_1, 1.0)));
                } else {
                    row_1 = ((double[])(appendDouble(row_1, 0.0)));
                }
                k_1 = k_1 + 1;
            }
            aug_1 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(aug_1), java.util.stream.Stream.of(row_1)).toArray(double[][]::new)));
            i_1 = i_1 + 1;
        }
        long col_1 = 0L;
        while (col_1 < n) {
            long pivot_row_1 = col_1;
            long r_1 = col_1;
            while (r_1 < n) {
                if (aug_1[(int)((long)(r_1))][(int)((long)(col_1))] != 0.0) {
                    pivot_row_1 = r_1;
                    break;
                }
                r_1 = r_1 + 1;
            }
            if (aug_1[(int)((long)(pivot_row_1))][(int)((long)(col_1))] == 0.0) {
                throw new RuntimeException(String.valueOf("Matrix is not invertible"));
            }
            if (pivot_row_1 != col_1) {
                double[] temp_1 = ((double[])(aug_1[(int)((long)(col_1))]));
aug_1[(int)((long)(col_1))] = ((double[])(aug_1[(int)((long)(pivot_row_1))]));
aug_1[(int)((long)(pivot_row_1))] = ((double[])(temp_1));
            }
            double pivot_1 = aug_1[(int)((long)(col_1))][(int)((long)(col_1))];
            long c_1 = 0L;
            while (c_1 < 2 * n) {
aug_1[(int)((long)(col_1))][(int)((long)(c_1))] = aug_1[(int)((long)(col_1))][(int)((long)(c_1))] / pivot_1;
                c_1 = c_1 + 1;
            }
            long r2_1 = 0L;
            while (r2_1 < n) {
                if (r2_1 != col_1) {
                    double factor_1 = aug_1[(int)((long)(r2_1))][(int)((long)(col_1))];
                    long c2_1 = 0L;
                    while (c2_1 < 2 * n) {
aug_1[(int)((long)(r2_1))][(int)((long)(c2_1))] = aug_1[(int)((long)(r2_1))][(int)((long)(c2_1))] - factor_1 * aug_1[(int)((long)(col_1))][(int)((long)(c2_1))];
                        c2_1 = c2_1 + 1;
                    }
                }
                r2_1 = r2_1 + 1;
            }
            col_1 = col_1 + 1;
        }
        double[][] inv_1 = ((double[][])(new double[][]{}));
        long r3_1 = 0L;
        while (r3_1 < n) {
            double[] row_3 = ((double[])(new double[]{}));
            long c3_1 = 0L;
            while (c3_1 < n) {
                row_3 = ((double[])(appendDouble(row_3, aug_1[(int)((long)(r3_1))][(int)((long)(c3_1 + n))])));
                c3_1 = c3_1 + 1;
            }
            inv_1 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(inv_1), java.util.stream.Stream.of(row_3)).toArray(double[][]::new)));
            r3_1 = r3_1 + 1;
        }
        return inv_1;
    }
    public static void main(String[] args) {
        mat = ((double[][])(new double[][]{new double[]{4.0, 7.0}, new double[]{2.0, 6.0}}));
        System.out.println("Original Matrix:");
        System.out.println(java.util.Arrays.deepToString(mat));
        System.out.println("Inverted Matrix:");
        System.out.println(invert_matrix(((double[][])(mat))));
    }

    static double[] appendDouble(double[] arr, double v) {
        double[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }
}
