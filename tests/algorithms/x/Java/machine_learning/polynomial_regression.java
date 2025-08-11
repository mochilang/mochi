public class Main {
    static double[] xs;
    static double[] ys = new double[0];
    static long i_10 = 0;
    static double[][] X = new double[0][];
    static double[][] Xt = new double[0][];
    static double[][] XtX = new double[0][];
    static double[] Xty = new double[0];
    static double[] coeffs;

    static double[][] design_matrix(double[] xs, long degree) {
        long i = 0;
        double[][] matrix_1 = ((double[][])(new double[][]{}));
        while (i < xs.length) {
            double[] row_1 = ((double[])(new double[]{}));
            long j_1 = 0;
            double pow_1 = 1.0;
            while (j_1 <= degree) {
                row_1 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row_1), java.util.stream.DoubleStream.of(pow_1)).toArray()));
                pow_1 = pow_1 * xs[(int)(i)];
                j_1 = j_1 + 1;
            }
            matrix_1 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(matrix_1), java.util.stream.Stream.of(row_1)).toArray(double[][]::new)));
            i = i + 1;
        }
        return matrix_1;
    }

    static double[][] transpose(double[][] matrix) {
        long rows = matrix.length;
        long cols_1 = matrix[(int)(0)].length;
        long j_3 = 0;
        double[][] result_1 = ((double[][])(new double[][]{}));
        while (j_3 < cols_1) {
            double[] row_3 = ((double[])(new double[]{}));
            long i_2 = 0;
            while (i_2 < rows) {
                row_3 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row_3), java.util.stream.DoubleStream.of(matrix[(int)(i_2)][(int)(j_3)])).toArray()));
                i_2 = i_2 + 1;
            }
            result_1 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(result_1), java.util.stream.Stream.of(row_3)).toArray(double[][]::new)));
            j_3 = j_3 + 1;
        }
        return result_1;
    }

    static double[][] matmul(double[][] A, double[][] B) {
        long n = A.length;
        long m_1 = A[(int)(0)].length;
        long p_1 = B[(int)(0)].length;
        long i_4 = 0;
        double[][] result_3 = ((double[][])(new double[][]{}));
        while (i_4 < n) {
            double[] row_5 = ((double[])(new double[]{}));
            long k_1 = 0;
            while (k_1 < p_1) {
                double sum_1 = 0.0;
                long j_5 = 0;
                while (j_5 < m_1) {
                    sum_1 = sum_1 + A[(int)(i_4)][(int)(j_5)] * B[(int)(j_5)][(int)(k_1)];
                    j_5 = j_5 + 1;
                }
                row_5 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row_5), java.util.stream.DoubleStream.of(sum_1)).toArray()));
                k_1 = k_1 + 1;
            }
            result_3 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(result_3), java.util.stream.Stream.of(row_5)).toArray(double[][]::new)));
            i_4 = i_4 + 1;
        }
        return result_3;
    }

    static double[] matvec_mul(double[][] A, double[] v) {
        long n_1 = A.length;
        long m_3 = A[(int)(0)].length;
        long i_6 = 0;
        double[] result_5 = ((double[])(new double[]{}));
        while (i_6 < n_1) {
            double sum_3 = 0.0;
            long j_7 = 0;
            while (j_7 < m_3) {
                sum_3 = sum_3 + A[(int)(i_6)][(int)(j_7)] * v[(int)(j_7)];
                j_7 = j_7 + 1;
            }
            result_5 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(result_5), java.util.stream.DoubleStream.of(sum_3)).toArray()));
            i_6 = i_6 + 1;
        }
        return result_5;
    }

    static double[] gaussian_elimination(double[][] A, double[] b) {
        long n_2 = A.length;
        double[][] M_1 = ((double[][])(new double[][]{}));
        long i_8 = 0;
        while (i_8 < n_2) {
            M_1 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(M_1), java.util.stream.Stream.of(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(A[(int)(i_8)]), java.util.stream.DoubleStream.of(b[(int)(i_8)])).toArray())).toArray(double[][]::new)));
            i_8 = i_8 + 1;
        }
        long k_3 = 0;
        while (k_3 < n_2) {
            long j_9 = k_3 + 1;
            while (j_9 < n_2) {
                double factor_1 = M_1[(int)(j_9)][(int)(k_3)] / M_1[(int)(k_3)][(int)(k_3)];
                double[] rowj_1 = ((double[])(M_1[(int)(j_9)]));
                double[] rowk_1 = ((double[])(M_1[(int)(k_3)]));
                long l_1 = k_3;
                while (l_1 <= n_2) {
rowj_1[(int)(l_1)] = rowj_1[(int)(l_1)] - factor_1 * rowk_1[(int)(l_1)];
                    l_1 = l_1 + 1;
                }
M_1[(int)(j_9)] = ((double[])(rowj_1));
                j_9 = j_9 + 1;
            }
            k_3 = k_3 + 1;
        }
        double[] x_1 = ((double[])(new double[]{}));
        long t_1 = 0;
        while (t_1 < n_2) {
            x_1 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(x_1), java.util.stream.DoubleStream.of(0.0)).toArray()));
            t_1 = t_1 + 1;
        }
        long i2_1 = n_2 - 1;
        while (i2_1 >= 0) {
            double sum_5 = M_1[(int)(i2_1)][(int)(n_2)];
            long j2_1 = i2_1 + 1;
            while (j2_1 < n_2) {
                sum_5 = sum_5 - M_1[(int)(i2_1)][(int)(j2_1)] * x_1[(int)(j2_1)];
                j2_1 = j2_1 + 1;
            }
x_1[(int)(i2_1)] = sum_5 / M_1[(int)(i2_1)][(int)(i2_1)];
            i2_1 = i2_1 - 1;
        }
        return x_1;
    }

    static double[] predict(double[] xs, double[] coeffs) {
        long i_9 = 0;
        double[] result_7 = ((double[])(new double[]{}));
        while (i_9 < xs.length) {
            double x_3 = xs[(int)(i_9)];
            long j_11 = 0;
            double pow_3 = 1.0;
            double sum_7 = 0.0;
            while (j_11 < coeffs.length) {
                sum_7 = sum_7 + coeffs[(int)(j_11)] * pow_3;
                pow_3 = pow_3 * x_3;
                j_11 = j_11 + 1;
            }
            result_7 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(result_7), java.util.stream.DoubleStream.of(sum_7)).toArray()));
            i_9 = i_9 + 1;
        }
        return result_7;
    }
    public static void main(String[] args) {
        xs = ((double[])(new double[]{0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0}));
        ys = ((double[])(new double[]{}));
        i_10 = 0;
        while (i_10 < xs.length) {
            double x_4 = xs[(int)(i_10)];
            ys = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(ys), java.util.stream.DoubleStream.of(x_4 * x_4 * x_4 - 2.0 * x_4 * x_4 + 3.0 * x_4 - 5.0)).toArray()));
            i_10 = i_10 + 1;
        }
        X = ((double[][])(design_matrix(((double[])(xs)), 3)));
        Xt = ((double[][])(transpose(((double[][])(X)))));
        XtX = ((double[][])(matmul(((double[][])(Xt)), ((double[][])(X)))));
        Xty = ((double[])(matvec_mul(((double[][])(Xt)), ((double[])(ys)))));
        coeffs = ((double[])(gaussian_elimination(((double[][])(XtX)), ((double[])(Xty)))));
        System.out.println(_p(coeffs));
        System.out.println(_p(predict(((double[])(new double[]{-1.0})), ((double[])(coeffs)))));
        System.out.println(_p(predict(((double[])(new double[]{-2.0})), ((double[])(coeffs)))));
        System.out.println(_p(predict(((double[])(new double[]{6.0})), ((double[])(coeffs)))));
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
}
