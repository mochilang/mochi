public class Main {
    static double[] xs = ((double[])(new double[]{0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0}));
    static double[] ys = ((double[])(new double[]{}));
    static long i_10 = 0L;
    static double[][] X = new double[0][];
    static double[][] Xt = new double[0][];
    static double[][] XtX = new double[0][];
    static double[] Xty = new double[0];
    static double[] coeffs;

    static double[][] design_matrix(double[] xs, long degree) {
        long i = 0L;
        double[][] matrix_1 = ((double[][])(new double[][]{}));
        while ((long)(i) < (long)(xs.length)) {
            double[] row_1 = ((double[])(new double[]{}));
            long j_1 = 0L;
            double pow_1 = (double)(1.0);
            while ((long)(j_1) <= (long)(degree)) {
                row_1 = ((double[])(appendDouble(row_1, (double)(pow_1))));
                pow_1 = (double)((double)(pow_1) * (double)(xs[(int)((long)(i))]));
                j_1 = (long)((long)(j_1) + 1L);
            }
            matrix_1 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(matrix_1), java.util.stream.Stream.of(new double[][]{row_1})).toArray(double[][]::new)));
            i = (long)((long)(i) + 1L);
        }
        return matrix_1;
    }

    static double[][] transpose(double[][] matrix) {
        long rows = (long)(matrix.length);
        long cols_1 = (long)(matrix[(int)(0L)].length);
        long j_3 = 0L;
        double[][] result_1 = ((double[][])(new double[][]{}));
        while ((long)(j_3) < (long)(cols_1)) {
            double[] row_3 = ((double[])(new double[]{}));
            long i_2 = 0L;
            while ((long)(i_2) < (long)(rows)) {
                row_3 = ((double[])(appendDouble(row_3, (double)(matrix[(int)((long)(i_2))][(int)((long)(j_3))]))));
                i_2 = (long)((long)(i_2) + 1L);
            }
            result_1 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(result_1), java.util.stream.Stream.of(new double[][]{row_3})).toArray(double[][]::new)));
            j_3 = (long)((long)(j_3) + 1L);
        }
        return result_1;
    }

    static double[][] matmul(double[][] A, double[][] B) {
        long n = (long)(A.length);
        long m_1 = (long)(A[(int)(0L)].length);
        long p_1 = (long)(B[(int)(0L)].length);
        long i_4 = 0L;
        double[][] result_3 = ((double[][])(new double[][]{}));
        while ((long)(i_4) < (long)(n)) {
            double[] row_5 = ((double[])(new double[]{}));
            long k_1 = 0L;
            while ((long)(k_1) < (long)(p_1)) {
                double sum_1 = (double)(0.0);
                long j_5 = 0L;
                while ((long)(j_5) < (long)(m_1)) {
                    sum_1 = (double)((double)(sum_1) + (double)((double)(A[(int)((long)(i_4))][(int)((long)(j_5))]) * (double)(B[(int)((long)(j_5))][(int)((long)(k_1))])));
                    j_5 = (long)((long)(j_5) + 1L);
                }
                row_5 = ((double[])(appendDouble(row_5, (double)(sum_1))));
                k_1 = (long)((long)(k_1) + 1L);
            }
            result_3 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(result_3), java.util.stream.Stream.of(new double[][]{row_5})).toArray(double[][]::new)));
            i_4 = (long)((long)(i_4) + 1L);
        }
        return result_3;
    }

    static double[] matvec_mul(double[][] A, double[] v) {
        long n_1 = (long)(A.length);
        long m_3 = (long)(A[(int)(0L)].length);
        long i_6 = 0L;
        double[] result_5 = ((double[])(new double[]{}));
        while ((long)(i_6) < (long)(n_1)) {
            double sum_3 = (double)(0.0);
            long j_7 = 0L;
            while ((long)(j_7) < (long)(m_3)) {
                sum_3 = (double)((double)(sum_3) + (double)((double)(A[(int)((long)(i_6))][(int)((long)(j_7))]) * (double)(v[(int)((long)(j_7))])));
                j_7 = (long)((long)(j_7) + 1L);
            }
            result_5 = ((double[])(appendDouble(result_5, (double)(sum_3))));
            i_6 = (long)((long)(i_6) + 1L);
        }
        return result_5;
    }

    static double[] gaussian_elimination(double[][] A, double[] b) {
        long n_2 = (long)(A.length);
        double[][] M_1 = ((double[][])(new double[][]{}));
        long i_8 = 0L;
        while ((long)(i_8) < (long)(n_2)) {
            M_1 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(M_1), java.util.stream.Stream.of(new double[][]{appendDouble(A[(int)((long)(i_8))], (double)(b[(int)((long)(i_8))]))})).toArray(double[][]::new)));
            i_8 = (long)((long)(i_8) + 1L);
        }
        long k_3 = 0L;
        while ((long)(k_3) < (long)(n_2)) {
            long j_9 = (long)((long)(k_3) + 1L);
            while ((long)(j_9) < (long)(n_2)) {
                double factor_1 = (double)((double)(M_1[(int)((long)(j_9))][(int)((long)(k_3))]) / (double)(M_1[(int)((long)(k_3))][(int)((long)(k_3))]));
                double[] rowj_1 = ((double[])(M_1[(int)((long)(j_9))]));
                double[] rowk_1 = ((double[])(M_1[(int)((long)(k_3))]));
                long l_1 = (long)(k_3);
                while ((long)(l_1) <= (long)(n_2)) {
rowj_1[(int)((long)(l_1))] = (double)((double)(rowj_1[(int)((long)(l_1))]) - (double)((double)(factor_1) * (double)(rowk_1[(int)((long)(l_1))])));
                    l_1 = (long)((long)(l_1) + 1L);
                }
M_1[(int)((long)(j_9))] = ((double[])(rowj_1));
                j_9 = (long)((long)(j_9) + 1L);
            }
            k_3 = (long)((long)(k_3) + 1L);
        }
        double[] x_1 = ((double[])(new double[]{}));
        long t_1 = 0L;
        while ((long)(t_1) < (long)(n_2)) {
            x_1 = ((double[])(appendDouble(x_1, (double)(0.0))));
            t_1 = (long)((long)(t_1) + 1L);
        }
        long i2_1 = (long)((long)(n_2) - 1L);
        while ((long)(i2_1) >= 0L) {
            double sum_5 = (double)(M_1[(int)((long)(i2_1))][(int)((long)(n_2))]);
            long j2_1 = (long)((long)(i2_1) + 1L);
            while ((long)(j2_1) < (long)(n_2)) {
                sum_5 = (double)((double)(sum_5) - (double)((double)(M_1[(int)((long)(i2_1))][(int)((long)(j2_1))]) * (double)(x_1[(int)((long)(j2_1))])));
                j2_1 = (long)((long)(j2_1) + 1L);
            }
x_1[(int)((long)(i2_1))] = (double)((double)(sum_5) / (double)(M_1[(int)((long)(i2_1))][(int)((long)(i2_1))]));
            i2_1 = (long)((long)(i2_1) - 1L);
        }
        return x_1;
    }

    static double[] predict(double[] xs, double[] coeffs) {
        long i_9 = 0L;
        double[] result_7 = ((double[])(new double[]{}));
        while ((long)(i_9) < (long)(xs.length)) {
            double x_3 = (double)(xs[(int)((long)(i_9))]);
            long j_11 = 0L;
            double pow_3 = (double)(1.0);
            double sum_7 = (double)(0.0);
            while ((long)(j_11) < (long)(coeffs.length)) {
                sum_7 = (double)((double)(sum_7) + (double)((double)(coeffs[(int)((long)(j_11))]) * (double)(pow_3)));
                pow_3 = (double)((double)(pow_3) * (double)(x_3));
                j_11 = (long)((long)(j_11) + 1L);
            }
            result_7 = ((double[])(appendDouble(result_7, (double)(sum_7))));
            i_9 = (long)((long)(i_9) + 1L);
        }
        return result_7;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            while ((long)(i_10) < (long)(xs.length)) {
                double x_4 = (double)(xs[(int)((long)(i_10))]);
                ys = ((double[])(appendDouble(ys, (double)((double)((double)((double)((double)((double)(x_4) * (double)(x_4)) * (double)(x_4)) - (double)((double)((double)(2.0) * (double)(x_4)) * (double)(x_4))) + (double)((double)(3.0) * (double)(x_4))) - (double)(5.0)))));
                i_10 = (long)((long)(i_10) + 1L);
            }
            X = ((double[][])(design_matrix(((double[])(xs)), 3L)));
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

    static double[] appendDouble(double[] arr, double v) {
        double[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
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
        if (v instanceof Double || v instanceof Float) {
            double d = ((Number) v).doubleValue();
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
