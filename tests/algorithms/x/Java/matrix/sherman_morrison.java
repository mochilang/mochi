public class Main {
    static class Matrix {
        double[][] data;
        long rows;
        long cols;
        Matrix(double[][] data, long rows, long cols) {
            this.data = data;
            this.rows = rows;
            this.cols = cols;
        }
        Matrix() {}
        @Override public String toString() {
            return String.format("{'data': %s, 'rows': %s, 'cols': %s}", String.valueOf(data), String.valueOf(rows), String.valueOf(cols));
        }
    }


    static Matrix make_matrix(long rows, long cols, double value) {
        double[][] arr = ((double[][])(new double[][]{}));
        long r_1 = 0L;
        while ((long)(r_1) < (long)(rows)) {
            double[] row_1 = ((double[])(new double[]{}));
            long c_1 = 0L;
            while ((long)(c_1) < (long)(cols)) {
                row_1 = ((double[])(appendDouble(row_1, (double)(value))));
                c_1 = (long)((long)(c_1) + 1L);
            }
            arr = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(arr), java.util.stream.Stream.of(new double[][]{row_1})).toArray(double[][]::new)));
            r_1 = (long)((long)(r_1) + 1L);
        }
        return new Matrix(arr, rows, cols);
    }

    static Matrix matrix_from_lists(double[][] vals) {
        long r_2 = (long)(vals.length);
        long c_3 = (long)((long)(r_2) == 0L ? 0 : vals[(int)((long)(0))].length);
        return new Matrix(vals, r_2, c_3);
    }

    static String matrix_to_string(Matrix m) {
        String s = "";
        long i_1 = 0L;
        while ((long)(i_1) < (long)(m.rows)) {
            s = s + "[";
            long j_1 = 0L;
            while ((long)(j_1) < (long)(m.cols)) {
                s = s + _p(_getd(m.data[(int)((long)(i_1))], ((Number)(j_1)).intValue()));
                if ((long)(j_1) < (long)((long)(m.cols) - 1L)) {
                    s = s + ", ";
                }
                j_1 = (long)((long)(j_1) + 1L);
            }
            s = s + "]";
            if ((long)(i_1) < (long)((long)(m.rows) - 1L)) {
                s = s + "\n";
            }
            i_1 = (long)((long)(i_1) + 1L);
        }
        return s;
    }

    static Matrix matrix_add(Matrix a, Matrix b) {
        if ((long)(a.rows) != (long)(b.rows) || (long)(a.cols) != (long)(b.cols)) {
            return new Matrix(new double[][]{}, 0, 0);
        }
        double[][] res_1 = ((double[][])(new double[][]{}));
        long i_3 = 0L;
        while ((long)(i_3) < (long)(a.rows)) {
            double[] row_3 = ((double[])(new double[]{}));
            long j_3 = 0L;
            while ((long)(j_3) < (long)(a.cols)) {
                row_3 = ((double[])(appendDouble(row_3, (double)((double)(a.data[(int)((long)(i_3))][(int)((long)(j_3))]) + (double)(b.data[(int)((long)(i_3))][(int)((long)(j_3))])))));
                j_3 = (long)((long)(j_3) + 1L);
            }
            res_1 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(res_1), java.util.stream.Stream.of(new double[][]{row_3})).toArray(double[][]::new)));
            i_3 = (long)((long)(i_3) + 1L);
        }
        return new Matrix(res_1, a.rows, a.cols);
    }

    static Matrix matrix_sub(Matrix a, Matrix b) {
        if ((long)(a.rows) != (long)(b.rows) || (long)(a.cols) != (long)(b.cols)) {
            return new Matrix(new double[][]{}, 0, 0);
        }
        double[][] res_3 = ((double[][])(new double[][]{}));
        long i_5 = 0L;
        while ((long)(i_5) < (long)(a.rows)) {
            double[] row_5 = ((double[])(new double[]{}));
            long j_5 = 0L;
            while ((long)(j_5) < (long)(a.cols)) {
                row_5 = ((double[])(appendDouble(row_5, (double)((double)(a.data[(int)((long)(i_5))][(int)((long)(j_5))]) - (double)(b.data[(int)((long)(i_5))][(int)((long)(j_5))])))));
                j_5 = (long)((long)(j_5) + 1L);
            }
            res_3 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(res_3), java.util.stream.Stream.of(new double[][]{row_5})).toArray(double[][]::new)));
            i_5 = (long)((long)(i_5) + 1L);
        }
        return new Matrix(res_3, a.rows, a.cols);
    }

    static Matrix matrix_mul_scalar(Matrix m, double k) {
        double[][] res_4 = ((double[][])(new double[][]{}));
        long i_7 = 0L;
        while ((long)(i_7) < (long)(m.rows)) {
            double[] row_7 = ((double[])(new double[]{}));
            long j_7 = 0L;
            while ((long)(j_7) < (long)(m.cols)) {
                row_7 = ((double[])(appendDouble(row_7, (double)((double)(m.data[(int)((long)(i_7))][(int)((long)(j_7))]) * (double)(k)))));
                j_7 = (long)((long)(j_7) + 1L);
            }
            res_4 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(res_4), java.util.stream.Stream.of(new double[][]{row_7})).toArray(double[][]::new)));
            i_7 = (long)((long)(i_7) + 1L);
        }
        return new Matrix(res_4, m.rows, m.cols);
    }

    static Matrix matrix_mul(Matrix a, Matrix b) {
        if ((long)(a.cols) != (long)(b.rows)) {
            return new Matrix(new double[][]{}, 0, 0);
        }
        double[][] res_6 = ((double[][])(new double[][]{}));
        long i_9 = 0L;
        while ((long)(i_9) < (long)(a.rows)) {
            double[] row_9 = ((double[])(new double[]{}));
            long j_9 = 0L;
            while ((long)(j_9) < (long)(b.cols)) {
                double sum_1 = (double)(0.0);
                long k_1 = 0L;
                while ((long)(k_1) < (long)(a.cols)) {
                    sum_1 = (double)((double)(sum_1) + (double)((double)(a.data[(int)((long)(i_9))][(int)((long)(k_1))]) * (double)(b.data[(int)((long)(k_1))][(int)((long)(j_9))])));
                    k_1 = (long)((long)(k_1) + 1L);
                }
                row_9 = ((double[])(appendDouble(row_9, (double)(sum_1))));
                j_9 = (long)((long)(j_9) + 1L);
            }
            res_6 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(res_6), java.util.stream.Stream.of(new double[][]{row_9})).toArray(double[][]::new)));
            i_9 = (long)((long)(i_9) + 1L);
        }
        return new Matrix(res_6, a.rows, b.cols);
    }

    static Matrix matrix_transpose(Matrix m) {
        double[][] res_7 = ((double[][])(new double[][]{}));
        long c_5 = 0L;
        while ((long)(c_5) < (long)(m.cols)) {
            double[] row_11 = ((double[])(new double[]{}));
            long r_4 = 0L;
            while ((long)(r_4) < (long)(m.rows)) {
                row_11 = ((double[])(appendDouble(row_11, (double)(m.data[(int)((long)(r_4))][(int)((long)(c_5))]))));
                r_4 = (long)((long)(r_4) + 1L);
            }
            res_7 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(res_7), java.util.stream.Stream.of(new double[][]{row_11})).toArray(double[][]::new)));
            c_5 = (long)((long)(c_5) + 1L);
        }
        return new Matrix(res_7, m.cols, m.rows);
    }

    static Matrix sherman_morrison(Matrix ainv, Matrix u, Matrix v) {
        Matrix vt = matrix_transpose(v);
        Matrix vu_1 = matrix_mul(matrix_mul(vt, ainv), u);
        double factor_1 = (double)((double)(vu_1.data[(int)((long)(0))][(int)((long)(0))]) + (double)(1.0));
        if ((double)(factor_1) == (double)(0.0)) {
            return new Matrix(new double[][]{}, 0, 0);
        }
        Matrix term1_1 = matrix_mul(ainv, u);
        Matrix term2_1 = matrix_mul(vt, ainv);
        Matrix numerator_1 = matrix_mul(term1_1, term2_1);
        Matrix scaled_1 = matrix_mul_scalar(numerator_1, (double)((double)(1.0) / (double)(factor_1)));
        return matrix_sub(ainv, scaled_1);
    }

    static void main() {
        Matrix ainv = matrix_from_lists(((double[][])(new double[][]{new double[]{1.0, 0.0, 0.0}, new double[]{0.0, 1.0, 0.0}, new double[]{0.0, 0.0, 1.0}})));
        Matrix u_1 = matrix_from_lists(((double[][])(new double[][]{new double[]{1.0}, new double[]{2.0}, new double[]{-3.0}})));
        Matrix v_1 = matrix_from_lists(((double[][])(new double[][]{new double[]{4.0}, new double[]{-2.0}, new double[]{5.0}})));
        Matrix result_1 = sherman_morrison(ainv, u_1, v_1);
        System.out.println(matrix_to_string(result_1));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            main();
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

    static Double _getd(double[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
