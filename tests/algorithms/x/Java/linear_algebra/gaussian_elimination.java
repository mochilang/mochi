public class Main {

    static double[][] retroactive_resolution(double[][] coefficients, double[][] vector) {
        long rows = coefficients.length;
        double[][] x_1 = ((double[][])(new double[][]{}));
        long i_1 = 0L;
        while (i_1 < rows) {
            double[] inner_1 = ((double[])(new double[]{}));
            inner_1 = ((double[])(appendDouble(inner_1, 0.0)));
            x_1 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(x_1), java.util.stream.Stream.of(inner_1)).toArray(double[][]::new)));
            i_1 = i_1 + 1;
        }
        long r_1 = rows - 1;
        while (r_1 >= 0) {
            double total_1 = 0.0;
            long c_1 = r_1 + 1;
            while (c_1 < rows) {
                total_1 = total_1 + coefficients[(int)((long)(r_1))][(int)((long)(c_1))] * x_1[(int)((long)(c_1))][(int)((long)(0))];
                c_1 = c_1 + 1;
            }
x_1[(int)((long)(r_1))][(int)((long)(0))] = (vector[(int)((long)(r_1))][(int)((long)(0))] - total_1) / coefficients[(int)((long)(r_1))][(int)((long)(r_1))];
            r_1 = r_1 - 1;
        }
        return x_1;
    }

    static double[][] gaussian_elimination(double[][] coefficients, double[][] vector) {
        long rows_1 = coefficients.length;
        long columns_1 = coefficients[(int)((long)(0))].length;
        if (rows_1 != columns_1) {
            return new double[][]{};
        }
        double[][] augmented_1 = ((double[][])(new double[][]{}));
        long i_3 = 0L;
        while (i_3 < rows_1) {
            double[] row_1 = ((double[])(new double[]{}));
            long j_1 = 0L;
            while (j_1 < columns_1) {
                row_1 = ((double[])(appendDouble(row_1, coefficients[(int)((long)(i_3))][(int)((long)(j_1))])));
                j_1 = j_1 + 1;
            }
            row_1 = ((double[])(appendDouble(row_1, vector[(int)((long)(i_3))][(int)((long)(0))])));
            augmented_1 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(augmented_1), java.util.stream.Stream.of(row_1)).toArray(double[][]::new)));
            i_3 = i_3 + 1;
        }
        long row_idx_1 = 0L;
        while (row_idx_1 < rows_1 - 1) {
            double pivot_1 = augmented_1[(int)((long)(row_idx_1))][(int)((long)(row_idx_1))];
            long col_1 = row_idx_1 + 1;
            while (col_1 < rows_1) {
                double factor_1 = augmented_1[(int)((long)(col_1))][(int)((long)(row_idx_1))] / pivot_1;
                long k_1 = row_idx_1;
                while (k_1 < columns_1 + 1) {
augmented_1[(int)((long)(col_1))][(int)((long)(k_1))] = augmented_1[(int)((long)(col_1))][(int)((long)(k_1))] - factor_1 * augmented_1[(int)((long)(row_idx_1))][(int)((long)(k_1))];
                    k_1 = k_1 + 1;
                }
                col_1 = col_1 + 1;
            }
            row_idx_1 = row_idx_1 + 1;
        }
        double[][] coeffs_1 = ((double[][])(new double[][]{}));
        double[][] vec_1 = ((double[][])(new double[][]{}));
        long r_3 = 0L;
        while (r_3 < rows_1) {
            double[] row_3 = ((double[])(new double[]{}));
            long c_3 = 0L;
            while (c_3 < columns_1) {
                row_3 = ((double[])(appendDouble(row_3, augmented_1[(int)((long)(r_3))][(int)((long)(c_3))])));
                c_3 = c_3 + 1;
            }
            coeffs_1 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(coeffs_1), java.util.stream.Stream.of(row_3)).toArray(double[][]::new)));
            vec_1 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(vec_1), java.util.stream.Stream.of(new double[]{augmented_1[(int)((long)(r_3))][(int)((long)(columns_1))]})).toArray(double[][]::new)));
            r_3 = r_3 + 1;
        }
        double[][] x_3 = ((double[][])(retroactive_resolution(((double[][])(coeffs_1)), ((double[][])(vec_1)))));
        return x_3;
    }
    public static void main(String[] args) {
        System.out.println(gaussian_elimination(((double[][])(new double[][]{new double[]{1.0, -4.0, -2.0}, new double[]{5.0, 2.0, -2.0}, new double[]{1.0, -1.0, 0.0}})), ((double[][])(new double[][]{new double[]{-2.0}, new double[]{-3.0}, new double[]{4.0}}))));
        System.out.println(gaussian_elimination(((double[][])(new double[][]{new double[]{1.0, 2.0}, new double[]{5.0, 2.0}})), ((double[][])(new double[][]{new double[]{5.0}, new double[]{5.0}}))));
    }

    static double[] appendDouble(double[] arr, double v) {
        double[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }
}
