public class Main {
    static double[] xs;
    static double[] ys = new double[0];
    static int i_6 = 0;
    static double[][] X = new double[0][];
    static double[][] Xt = new double[0][];
    static double[][] XtX = new double[0][];
    static double[] Xty = new double[0];
    static double[] coeffs;

    static double[][] design_matrix(double[] xs, int degree) {
        int i = 0;
        double[][] matrix = ((double[][])(new double[][]{}));
        while (i < xs.length) {
            double[] row = ((double[])(new double[]{}));
            int j = 0;
            double pow = 1.0;
            while (j <= degree) {
                row = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row), java.util.stream.DoubleStream.of(pow)).toArray()));
                pow = pow * xs[i];
                j = j + 1;
            }
            matrix = ((double[][])(appendObj(matrix, row)));
            i = i + 1;
        }
        return matrix;
    }

    static double[][] transpose(double[][] matrix) {
        int rows = matrix.length;
        int cols = matrix[0].length;
        int j_1 = 0;
        double[][] result = ((double[][])(new double[][]{}));
        while (j_1 < cols) {
            double[] row_1 = ((double[])(new double[]{}));
            int i_1 = 0;
            while (i_1 < rows) {
                row_1 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row_1), java.util.stream.DoubleStream.of(matrix[i_1][j_1])).toArray()));
                i_1 = i_1 + 1;
            }
            result = ((double[][])(appendObj(result, row_1)));
            j_1 = j_1 + 1;
        }
        return result;
    }

    static double[][] matmul(double[][] A, double[][] B) {
        int n = A.length;
        int m = A[0].length;
        int p = B[0].length;
        int i_2 = 0;
        double[][] result_1 = ((double[][])(new double[][]{}));
        while (i_2 < n) {
            double[] row_2 = ((double[])(new double[]{}));
            int k = 0;
            while (k < p) {
                double sum = 0.0;
                int j_2 = 0;
                while (j_2 < m) {
                    sum = sum + A[i_2][j_2] * B[j_2][k];
                    j_2 = j_2 + 1;
                }
                row_2 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row_2), java.util.stream.DoubleStream.of(sum)).toArray()));
                k = k + 1;
            }
            result_1 = ((double[][])(appendObj(result_1, row_2)));
            i_2 = i_2 + 1;
        }
        return result_1;
    }

    static double[] matvec_mul(double[][] A, double[] v) {
        int n_1 = A.length;
        int m_1 = A[0].length;
        int i_3 = 0;
        double[] result_2 = ((double[])(new double[]{}));
        while (i_3 < n_1) {
            double sum_1 = 0.0;
            int j_3 = 0;
            while (j_3 < m_1) {
                sum_1 = sum_1 + A[i_3][j_3] * v[j_3];
                j_3 = j_3 + 1;
            }
            result_2 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(result_2), java.util.stream.DoubleStream.of(sum_1)).toArray()));
            i_3 = i_3 + 1;
        }
        return result_2;
    }

    static double[] gaussian_elimination(double[][] A, double[] b) {
        int n_2 = A.length;
        double[][] M = ((double[][])(new double[][]{}));
        int i_4 = 0;
        while (i_4 < n_2) {
            M = ((double[][])(appendObj(M, java.util.stream.DoubleStream.concat(java.util.Arrays.stream(A[i_4]), java.util.stream.DoubleStream.of(b[i_4])).toArray())));
            i_4 = i_4 + 1;
        }
        int k_1 = 0;
        while (k_1 < n_2) {
            int j_4 = k_1 + 1;
            while (j_4 < n_2) {
                double factor = M[j_4][k_1] / M[k_1][k_1];
                double[] rowj = ((double[])(M[j_4]));
                double[] rowk = ((double[])(M[k_1]));
                int l = k_1;
                while (l <= n_2) {
rowj[l] = rowj[l] - factor * rowk[l];
                    l = l + 1;
                }
M[j_4] = ((double[])(rowj));
                j_4 = j_4 + 1;
            }
            k_1 = k_1 + 1;
        }
        double[] x = ((double[])(new double[]{}));
        int t = 0;
        while (t < n_2) {
            x = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(x), java.util.stream.DoubleStream.of(0.0)).toArray()));
            t = t + 1;
        }
        int i2 = n_2 - 1;
        while (i2 >= 0) {
            double sum_2 = M[i2][n_2];
            int j2 = i2 + 1;
            while (j2 < n_2) {
                sum_2 = sum_2 - M[i2][j2] * x[j2];
                j2 = j2 + 1;
            }
x[i2] = sum_2 / M[i2][i2];
            i2 = i2 - 1;
        }
        return x;
    }

    static double[] predict(double[] xs, double[] coeffs) {
        int i_5 = 0;
        double[] result_3 = ((double[])(new double[]{}));
        while (i_5 < xs.length) {
            double x_1 = xs[i_5];
            int j_5 = 0;
            double pow_1 = 1.0;
            double sum_3 = 0.0;
            while (j_5 < coeffs.length) {
                sum_3 = sum_3 + coeffs[j_5] * pow_1;
                pow_1 = pow_1 * x_1;
                j_5 = j_5 + 1;
            }
            result_3 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(result_3), java.util.stream.DoubleStream.of(sum_3)).toArray()));
            i_5 = i_5 + 1;
        }
        return result_3;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            xs = ((double[])(new double[]{0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0}));
            ys = ((double[])(new double[]{}));
            i_6 = 0;
            while (i_6 < xs.length) {
                double x_2 = xs[i_6];
                ys = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(ys), java.util.stream.DoubleStream.of(x_2 * x_2 * x_2 - 2.0 * x_2 * x_2 + 3.0 * x_2 - 5.0)).toArray()));
                i_6 = i_6 + 1;
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
        return String.valueOf(v);
    }
}
