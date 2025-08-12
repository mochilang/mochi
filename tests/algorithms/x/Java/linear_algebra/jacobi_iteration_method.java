public class Main {
    static double[][] coefficient;
    static double[] constant;
    static double[] init_val;
    static long iterations;
    static double[] result;

    static double absf(double x) {
        return x < 0.0 ? -x : x;
    }

    static boolean strictly_diagonally_dominant(double[][] matrix) {
        long n = matrix.length;
        long i_1 = 0L;
        while (i_1 < n) {
            double sum_1 = 0.0;
            long j_1 = 0L;
            while (j_1 < n) {
                if (i_1 != j_1) {
                    sum_1 = sum_1 + absf(matrix[(int)((long)(i_1))][(int)((long)(j_1))]);
                }
                j_1 = j_1 + 1;
            }
            if (absf(matrix[(int)((long)(i_1))][(int)((long)(i_1))]) <= sum_1) {
                throw new RuntimeException(String.valueOf("Coefficient matrix is not strictly diagonally dominant"));
            }
            i_1 = i_1 + 1;
        }
        return true;
    }

    static double[] jacobi_iteration_method(double[][] coefficient, double[] constant, double[] init_val, long iterations) {
        long n_1 = coefficient.length;
        if (n_1 == 0) {
            throw new RuntimeException(String.valueOf("Coefficient matrix cannot be empty"));
        }
        if (constant.length != n_1) {
            throw new RuntimeException(String.valueOf("Constant vector length must equal number of rows in coefficient matrix"));
        }
        if (init_val.length != n_1) {
            throw new RuntimeException(String.valueOf("Initial values count must match matrix size"));
        }
        long r_1 = 0L;
        while (r_1 < n_1) {
            if (coefficient[(int)((long)(r_1))].length != n_1) {
                throw new RuntimeException(String.valueOf("Coefficient matrix must be square"));
            }
            r_1 = r_1 + 1;
        }
        if (iterations <= 0) {
            throw new RuntimeException(String.valueOf("Iterations must be at least 1"));
        }
        strictly_diagonally_dominant(((double[][])(coefficient)));
        double[] x_1 = ((double[])(init_val));
        long k_1 = 0L;
        while (k_1 < iterations) {
            double[] new_x_1 = ((double[])(new double[]{}));
            long i_3 = 0L;
            while (i_3 < n_1) {
                double sum_3 = 0.0;
                long j_3 = 0L;
                while (j_3 < n_1) {
                    if (i_3 != j_3) {
                        sum_3 = sum_3 + coefficient[(int)((long)(i_3))][(int)((long)(j_3))] * x_1[(int)((long)(j_3))];
                    }
                    j_3 = j_3 + 1;
                }
                double value_1 = (constant[(int)((long)(i_3))] - sum_3) / coefficient[(int)((long)(i_3))][(int)((long)(i_3))];
                new_x_1 = ((double[])(appendDouble(new_x_1, value_1)));
                i_3 = i_3 + 1;
            }
            x_1 = ((double[])(new_x_1));
            k_1 = k_1 + 1;
        }
        return x_1;
    }
    public static void main(String[] args) {
        coefficient = ((double[][])(new double[][]{new double[]{4.0, 1.0, 1.0}, new double[]{1.0, 5.0, 2.0}, new double[]{1.0, 2.0, 4.0}}));
        constant = ((double[])(new double[]{2.0, -6.0, -4.0}));
        init_val = ((double[])(new double[]{0.5, -0.5, -0.5}));
        iterations = 3;
        result = ((double[])(jacobi_iteration_method(((double[][])(coefficient)), ((double[])(constant)), ((double[])(init_val)), iterations)));
        System.out.println(java.util.Arrays.toString(result));
    }

    static double[] appendDouble(double[] arr, double v) {
        double[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }
}
