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


    static Matrix make_matrix(double[][] values) {
        long r = (long)(values.length);
        if ((long)(r) == 0L) {
            return new Matrix(new double[][]{}, 0, 0);
        }
        long c_1 = (long)(values[(int)(0L)].length);
        long i_1 = 0L;
        while ((long)(i_1) < (long)(r)) {
            if ((long)(values[(int)((long)(i_1))].length) != (long)(c_1)) {
                return new Matrix(new double[][]{}, 0, 0);
            }
            i_1 = (long)((long)(i_1) + 1L);
        }
        return new Matrix(values, r, c_1);
    }

    static double[][] matrix_columns(Matrix m) {
        double[][] cols = ((double[][])(new double[][]{}));
        long j_1 = 0L;
        while ((long)(j_1) < (long)(m.cols)) {
            double[] col_1 = ((double[])(new double[]{}));
            long i_3 = 0L;
            while ((long)(i_3) < (long)(m.rows)) {
                col_1 = ((double[])(appendDouble(col_1, (double)(m.data[(int)((long)(i_3))][(int)((long)(j_1))]))));
                i_3 = (long)((long)(i_3) + 1L);
            }
            cols = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(cols), java.util.stream.Stream.of(new double[][]{col_1})).toArray(double[][]::new)));
            j_1 = (long)((long)(j_1) + 1L);
        }
        return cols;
    }

    static Matrix matrix_identity(Matrix m) {
        double[][] vals = ((double[][])(new double[][]{}));
        long i_5 = 0L;
        while ((long)(i_5) < (long)(m.rows)) {
            double[] row_1 = ((double[])(new double[]{}));
            long j_3 = 0L;
            while ((long)(j_3) < (long)(m.cols)) {
                double v_1 = (double)((long)(i_5) == (long)(j_3) ? 1.0 : 0.0);
                row_1 = ((double[])(appendDouble(row_1, (double)(v_1))));
                j_3 = (long)((long)(j_3) + 1L);
            }
            vals = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(vals), java.util.stream.Stream.of(new double[][]{row_1})).toArray(double[][]::new)));
            i_5 = (long)((long)(i_5) + 1L);
        }
        return new Matrix(vals, m.rows, m.cols);
    }

    static double matrix_minor(Matrix m, long r, long c) {
        double[][] vals_1 = ((double[][])(new double[][]{}));
        long i_7 = 0L;
        while ((long)(i_7) < (long)(m.rows)) {
            if ((long)(i_7) != (long)(r)) {
                double[] row_3 = ((double[])(new double[]{}));
                long j_5 = 0L;
                while ((long)(j_5) < (long)(m.cols)) {
                    if ((long)(j_5) != (long)(c)) {
                        row_3 = ((double[])(appendDouble(row_3, (double)(m.data[(int)((long)(i_7))][(int)((long)(j_5))]))));
                    }
                    j_5 = (long)((long)(j_5) + 1L);
                }
                vals_1 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(vals_1), java.util.stream.Stream.of(new double[][]{row_3})).toArray(double[][]::new)));
            }
            i_7 = (long)((long)(i_7) + 1L);
        }
        Matrix sub_1 = new Matrix(vals_1, (long)(m.rows) - 1L, (long)(m.cols) - 1L);
        return matrix_determinant(sub_1);
    }

    static double matrix_cofactor(Matrix m, long r, long c) {
        double minor = (double)(matrix_minor(m, (long)(r), (long)(c)));
        if (Math.floorMod(((long)(r) + (long)(c)), 2) == 0L) {
            return minor;
        }
        return (double)(-1.0) * (double)(minor);
    }

    static Matrix matrix_minors(Matrix m) {
        double[][] vals_2 = ((double[][])(new double[][]{}));
        long i_9 = 0L;
        while ((long)(i_9) < (long)(m.rows)) {
            double[] row_5 = ((double[])(new double[]{}));
            long j_7 = 0L;
            while ((long)(j_7) < (long)(m.cols)) {
                row_5 = ((double[])(appendDouble(row_5, (double)(matrix_minor(m, (long)(i_9), (long)(j_7))))));
                j_7 = (long)((long)(j_7) + 1L);
            }
            vals_2 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(vals_2), java.util.stream.Stream.of(new double[][]{row_5})).toArray(double[][]::new)));
            i_9 = (long)((long)(i_9) + 1L);
        }
        return new Matrix(vals_2, m.rows, m.cols);
    }

    static Matrix matrix_cofactors(Matrix m) {
        double[][] vals_3 = ((double[][])(new double[][]{}));
        long i_11 = 0L;
        while ((long)(i_11) < (long)(m.rows)) {
            double[] row_7 = ((double[])(new double[]{}));
            long j_9 = 0L;
            while ((long)(j_9) < (long)(m.cols)) {
                row_7 = ((double[])(appendDouble(row_7, (double)(matrix_cofactor(m, (long)(i_11), (long)(j_9))))));
                j_9 = (long)((long)(j_9) + 1L);
            }
            vals_3 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(vals_3), java.util.stream.Stream.of(new double[][]{row_7})).toArray(double[][]::new)));
            i_11 = (long)((long)(i_11) + 1L);
        }
        return new Matrix(vals_3, m.rows, m.cols);
    }

    static double matrix_determinant(Matrix m) {
        if ((long)(m.rows) != (long)(m.cols)) {
            return 0.0;
        }
        if ((long)(m.rows) == 0L) {
            return 0.0;
        }
        if ((long)(m.rows) == 1L) {
            return m.data[(int)(0L)][(int)(0L)];
        }
        if ((long)(m.rows) == 2L) {
            return (double)((double)(m.data[(int)(0L)][(int)(0L)]) * (double)(m.data[(int)(1L)][(int)(1L)])) - (double)((double)(m.data[(int)(0L)][(int)(1L)]) * (double)(m.data[(int)(1L)][(int)(0L)]));
        }
        double sum_1 = (double)(0.0);
        long j_11 = 0L;
        while ((long)(j_11) < (long)(m.cols)) {
            sum_1 = (double)((double)(sum_1) + (double)((double)(m.data[(int)(0L)][(int)((long)(j_11))]) * (double)(matrix_cofactor(m, 0L, (long)(j_11)))));
            j_11 = (long)((long)(j_11) + 1L);
        }
        return sum_1;
    }

    static boolean matrix_is_invertible(Matrix m) {
        return (double)(matrix_determinant(m)) != (double)(0.0);
    }

    static Matrix matrix_adjugate(Matrix m) {
        Matrix cof = matrix_cofactors(m);
        double[][] vals_5 = ((double[][])(new double[][]{}));
        long i_13 = 0L;
        while ((long)(i_13) < (long)(m.rows)) {
            double[] row_9 = ((double[])(new double[]{}));
            long j_13 = 0L;
            while ((long)(j_13) < (long)(m.cols)) {
                row_9 = ((double[])(appendDouble(row_9, (double)(cof.data[(int)((long)(j_13))][(int)((long)(i_13))]))));
                j_13 = (long)((long)(j_13) + 1L);
            }
            vals_5 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(vals_5), java.util.stream.Stream.of(new double[][]{row_9})).toArray(double[][]::new)));
            i_13 = (long)((long)(i_13) + 1L);
        }
        return new Matrix(vals_5, m.rows, m.cols);
    }

    static Matrix matrix_inverse(Matrix m) {
        double det = (double)(matrix_determinant(m));
        if ((double)(det) == (double)(0.0)) {
            return new Matrix(new double[][]{}, 0, 0);
        }
        Matrix adj_1 = matrix_adjugate(m);
        return matrix_mul_scalar(adj_1, (double)((double)(1.0) / (double)(det)));
    }

    static Matrix matrix_add_row(Matrix m, double[] row) {
        double[][] newData = ((double[][])(m.data));
        newData = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(newData), java.util.stream.Stream.of(new double[][]{row})).toArray(double[][]::new)));
        return new Matrix(newData, (long)(m.rows) + 1L, m.cols);
    }

    static Matrix matrix_add_column(Matrix m, double[] col) {
        double[][] newData_1 = ((double[][])(new double[][]{}));
        long i_15 = 0L;
        while ((long)(i_15) < (long)(m.rows)) {
            newData_1 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(newData_1), java.util.stream.Stream.of(new double[][]{appendDouble(m.data[(int)((long)(i_15))], (double)(col[(int)((long)(i_15))]))})).toArray(double[][]::new)));
            i_15 = (long)((long)(i_15) + 1L);
        }
        return new Matrix(newData_1, m.rows, (long)(m.cols) + 1L);
    }

    static Matrix matrix_mul_scalar(Matrix m, double s) {
        double[][] vals_6 = ((double[][])(new double[][]{}));
        long i_17 = 0L;
        while ((long)(i_17) < (long)(m.rows)) {
            double[] row_11 = ((double[])(new double[]{}));
            long j_15 = 0L;
            while ((long)(j_15) < (long)(m.cols)) {
                row_11 = ((double[])(appendDouble(row_11, (double)((double)(m.data[(int)((long)(i_17))][(int)((long)(j_15))]) * (double)(s)))));
                j_15 = (long)((long)(j_15) + 1L);
            }
            vals_6 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(vals_6), java.util.stream.Stream.of(new double[][]{row_11})).toArray(double[][]::new)));
            i_17 = (long)((long)(i_17) + 1L);
        }
        return new Matrix(vals_6, m.rows, m.cols);
    }

    static Matrix matrix_neg(Matrix m) {
        return matrix_mul_scalar(m, (double)(-1.0));
    }

    static Matrix matrix_add(Matrix a, Matrix b) {
        if ((long)(a.rows) != (long)(b.rows) || (long)(a.cols) != (long)(b.cols)) {
            return new Matrix(new double[][]{}, 0, 0);
        }
        double[][] vals_8 = ((double[][])(new double[][]{}));
        long i_19 = 0L;
        while ((long)(i_19) < (long)(a.rows)) {
            double[] row_13 = ((double[])(new double[]{}));
            long j_17 = 0L;
            while ((long)(j_17) < (long)(a.cols)) {
                row_13 = ((double[])(appendDouble(row_13, (double)((double)(a.data[(int)((long)(i_19))][(int)((long)(j_17))]) + (double)(b.data[(int)((long)(i_19))][(int)((long)(j_17))])))));
                j_17 = (long)((long)(j_17) + 1L);
            }
            vals_8 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(vals_8), java.util.stream.Stream.of(new double[][]{row_13})).toArray(double[][]::new)));
            i_19 = (long)((long)(i_19) + 1L);
        }
        return new Matrix(vals_8, a.rows, a.cols);
    }

    static Matrix matrix_sub(Matrix a, Matrix b) {
        if ((long)(a.rows) != (long)(b.rows) || (long)(a.cols) != (long)(b.cols)) {
            return new Matrix(new double[][]{}, 0, 0);
        }
        double[][] vals_10 = ((double[][])(new double[][]{}));
        long i_21 = 0L;
        while ((long)(i_21) < (long)(a.rows)) {
            double[] row_15 = ((double[])(new double[]{}));
            long j_19 = 0L;
            while ((long)(j_19) < (long)(a.cols)) {
                row_15 = ((double[])(appendDouble(row_15, (double)((double)(a.data[(int)((long)(i_21))][(int)((long)(j_19))]) - (double)(b.data[(int)((long)(i_21))][(int)((long)(j_19))])))));
                j_19 = (long)((long)(j_19) + 1L);
            }
            vals_10 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(vals_10), java.util.stream.Stream.of(new double[][]{row_15})).toArray(double[][]::new)));
            i_21 = (long)((long)(i_21) + 1L);
        }
        return new Matrix(vals_10, a.rows, a.cols);
    }

    static double matrix_dot(double[] row, double[] col) {
        double sum_2 = (double)(0.0);
        long i_23 = 0L;
        while ((long)(i_23) < (long)(row.length)) {
            sum_2 = (double)((double)(sum_2) + (double)((double)(row[(int)((long)(i_23))]) * (double)(col[(int)((long)(i_23))])));
            i_23 = (long)((long)(i_23) + 1L);
        }
        return sum_2;
    }

    static Matrix matrix_mul(Matrix a, Matrix b) {
        if ((long)(a.cols) != (long)(b.rows)) {
            return new Matrix(new double[][]{}, 0, 0);
        }
        double[][] bcols_1 = ((double[][])(matrix_columns(b)));
        double[][] vals_12 = ((double[][])(new double[][]{}));
        long i_25 = 0L;
        while ((long)(i_25) < (long)(a.rows)) {
            double[] row_17 = ((double[])(new double[]{}));
            long j_21 = 0L;
            while ((long)(j_21) < (long)(b.cols)) {
                row_17 = ((double[])(appendDouble(row_17, (double)(matrix_dot(((double[])(a.data[(int)((long)(i_25))])), ((double[])(bcols_1[(int)((long)(j_21))])))))));
                j_21 = (long)((long)(j_21) + 1L);
            }
            vals_12 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(vals_12), java.util.stream.Stream.of(new double[][]{row_17})).toArray(double[][]::new)));
            i_25 = (long)((long)(i_25) + 1L);
        }
        return new Matrix(vals_12, a.rows, b.cols);
    }

    static Matrix matrix_pow(Matrix m, long p) {
        if ((long)(p) == 0L) {
            return matrix_identity(m);
        }
        if ((long)(p) < 0L) {
            if (matrix_is_invertible(m)) {
                return matrix_pow(matrix_inverse(m), (long)(-p));
            }
            return new Matrix(new double[][]{}, 0, 0);
        }
        Matrix result_1 = m;
        long i_27 = 1L;
        while ((long)(i_27) < (long)(p)) {
            result_1 = matrix_mul(result_1, m);
            i_27 = (long)((long)(i_27) + 1L);
        }
        return result_1;
    }

    static String matrix_to_string(Matrix m) {
        if ((long)(m.rows) == 0L) {
            return "[]";
        }
        String s_1 = "[";
        long i_29 = 0L;
        while ((long)(i_29) < (long)(m.rows)) {
            s_1 = s_1 + "[";
            long j_23 = 0L;
            while ((long)(j_23) < (long)(m.cols)) {
                s_1 = s_1 + _p(_getd(m.data[(int)((long)(i_29))], ((Number)(j_23)).intValue()));
                if ((long)(j_23) < (long)((long)(m.cols) - 1L)) {
                    s_1 = s_1 + " ";
                }
                j_23 = (long)((long)(j_23) + 1L);
            }
            s_1 = s_1 + "]";
            if ((long)(i_29) < (long)((long)(m.rows) - 1L)) {
                s_1 = s_1 + "\n ";
            }
            i_29 = (long)((long)(i_29) + 1L);
        }
        s_1 = s_1 + "]";
        return s_1;
    }

    static void main() {
        Matrix m = make_matrix(((double[][])(new double[][]{new double[]{1.0, 2.0, 3.0}, new double[]{4.0, 5.0, 6.0}, new double[]{7.0, 8.0, 9.0}})));
        System.out.println(matrix_to_string(m));
        System.out.println(_p(matrix_columns(m)));
        System.out.println(_p(m.rows) + "," + _p(m.cols));
        System.out.println(_p(matrix_is_invertible(m)));
        System.out.println(matrix_to_string(matrix_identity(m)));
        System.out.println(_p(matrix_determinant(m)));
        System.out.println(matrix_to_string(matrix_minors(m)));
        System.out.println(matrix_to_string(matrix_cofactors(m)));
        System.out.println(matrix_to_string(matrix_adjugate(m)));
        Matrix m2_1 = matrix_mul_scalar(m, (double)(3.0));
        System.out.println(matrix_to_string(m2_1));
        System.out.println(matrix_to_string(matrix_add(m, m2_1)));
        System.out.println(matrix_to_string(matrix_sub(m, m2_1)));
        System.out.println(matrix_to_string(matrix_pow(m, 3L)));
        Matrix m3_1 = matrix_add_row(m, ((double[])(new double[]{10.0, 11.0, 12.0})));
        System.out.println(matrix_to_string(m3_1));
        Matrix m4_1 = matrix_add_column(m2_1, ((double[])(new double[]{8.0, 16.0, 32.0})));
        System.out.println(matrix_to_string(matrix_mul(m3_1, m4_1)));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            main();
            long _benchDuration = _now() - _benchStart;
            long _benchMemory = _mem() - _benchMem;
            System.out.println("{\"duration_us\": " + _benchDuration + ", \"memory_bytes\": " + _benchMemory + ", \"name\": \"main\"}");
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
