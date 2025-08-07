public class Main {
    static class Matrix {
        double[][] data;
        int rows;
        int cols;
        Matrix(double[][] data, int rows, int cols) {
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
        int r = values.length;
        if (r == 0) {
            return new Matrix(new double[][]{}, 0, 0);
        }
        int c = values[0].length;
        int i = 0;
        while (i < r) {
            if (values[i].length != c) {
                return new Matrix(new double[][]{}, 0, 0);
            }
            i = i + 1;
        }
        return new Matrix(values, r, c);
    }

    static double[][] matrix_columns(Matrix m) {
        double[][] cols = ((double[][])(new double[][]{}));
        int j = 0;
        while (j < m.cols) {
            double[] col = ((double[])(new double[]{}));
            int i_1 = 0;
            while (i_1 < m.rows) {
                col = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(col), java.util.stream.DoubleStream.of(m.data[i_1][j])).toArray()));
                i_1 = i_1 + 1;
            }
            cols = ((double[][])(appendObj(cols, col)));
            j = j + 1;
        }
        return cols;
    }

    static Matrix matrix_identity(Matrix m) {
        double[][] vals = ((double[][])(new double[][]{}));
        int i_2 = 0;
        while (i_2 < m.rows) {
            double[] row = ((double[])(new double[]{}));
            int j_1 = 0;
            while (j_1 < m.cols) {
                double v = i_2 == j_1 ? 1.0 : 0.0;
                row = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row), java.util.stream.DoubleStream.of(v)).toArray()));
                j_1 = j_1 + 1;
            }
            vals = ((double[][])(appendObj(vals, row)));
            i_2 = i_2 + 1;
        }
        return new Matrix(vals, m.rows, m.cols);
    }

    static double matrix_minor(Matrix m, int r, int c) {
        double[][] vals_1 = ((double[][])(new double[][]{}));
        int i_3 = 0;
        while (i_3 < m.rows) {
            if (i_3 != r) {
                double[] row_1 = ((double[])(new double[]{}));
                int j_2 = 0;
                while (j_2 < m.cols) {
                    if (j_2 != c) {
                        row_1 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row_1), java.util.stream.DoubleStream.of(m.data[i_3][j_2])).toArray()));
                    }
                    j_2 = j_2 + 1;
                }
                vals_1 = ((double[][])(appendObj(vals_1, row_1)));
            }
            i_3 = i_3 + 1;
        }
        Matrix sub = new Matrix(vals_1, m.rows - 1, m.cols - 1);
        return matrix_determinant(sub);
    }

    static double matrix_cofactor(Matrix m, int r, int c) {
        double minor = matrix_minor(m, r, c);
        if (Math.floorMod((r + c), 2) == 0) {
            return minor;
        }
        return -1.0 * minor;
    }

    static Matrix matrix_minors(Matrix m) {
        double[][] vals_2 = ((double[][])(new double[][]{}));
        int i_4 = 0;
        while (i_4 < m.rows) {
            double[] row_2 = ((double[])(new double[]{}));
            int j_3 = 0;
            while (j_3 < m.cols) {
                row_2 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row_2), java.util.stream.DoubleStream.of(matrix_minor(m, i_4, j_3))).toArray()));
                j_3 = j_3 + 1;
            }
            vals_2 = ((double[][])(appendObj(vals_2, row_2)));
            i_4 = i_4 + 1;
        }
        return new Matrix(vals_2, m.rows, m.cols);
    }

    static Matrix matrix_cofactors(Matrix m) {
        double[][] vals_3 = ((double[][])(new double[][]{}));
        int i_5 = 0;
        while (i_5 < m.rows) {
            double[] row_3 = ((double[])(new double[]{}));
            int j_4 = 0;
            while (j_4 < m.cols) {
                row_3 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row_3), java.util.stream.DoubleStream.of(matrix_cofactor(m, i_5, j_4))).toArray()));
                j_4 = j_4 + 1;
            }
            vals_3 = ((double[][])(appendObj(vals_3, row_3)));
            i_5 = i_5 + 1;
        }
        return new Matrix(vals_3, m.rows, m.cols);
    }

    static double matrix_determinant(Matrix m) {
        if (m.rows != m.cols) {
            return 0.0;
        }
        if (m.rows == 0) {
            return 0.0;
        }
        if (m.rows == 1) {
            return m.data[0][0];
        }
        if (m.rows == 2) {
            return m.data[0][0] * m.data[1][1] - m.data[0][1] * m.data[1][0];
        }
        double sum = 0.0;
        int j_5 = 0;
        while (j_5 < m.cols) {
            sum = sum + m.data[0][j_5] * matrix_cofactor(m, 0, j_5);
            j_5 = j_5 + 1;
        }
        return sum;
    }

    static boolean matrix_is_invertible(Matrix m) {
        return matrix_determinant(m) != 0.0;
    }

    static Matrix matrix_adjugate(Matrix m) {
        Matrix cof = matrix_cofactors(m);
        double[][] vals_4 = ((double[][])(new double[][]{}));
        int i_6 = 0;
        while (i_6 < m.rows) {
            double[] row_4 = ((double[])(new double[]{}));
            int j_6 = 0;
            while (j_6 < m.cols) {
                row_4 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row_4), java.util.stream.DoubleStream.of(cof.data[j_6][i_6])).toArray()));
                j_6 = j_6 + 1;
            }
            vals_4 = ((double[][])(appendObj(vals_4, row_4)));
            i_6 = i_6 + 1;
        }
        return new Matrix(vals_4, m.rows, m.cols);
    }

    static Matrix matrix_inverse(Matrix m) {
        double det = matrix_determinant(m);
        if (det == 0.0) {
            return new Matrix(new double[][]{}, 0, 0);
        }
        Matrix adj = matrix_adjugate(m);
        return matrix_mul_scalar(adj, 1.0 / det);
    }

    static Matrix matrix_add_row(Matrix m, double[] row) {
        double[][] newData = ((double[][])(m.data));
        newData = ((double[][])(appendObj(newData, row)));
        return new Matrix(newData, m.rows + 1, m.cols);
    }

    static Matrix matrix_add_column(Matrix m, double[] col) {
        double[][] newData_1 = ((double[][])(new double[][]{}));
        int i_7 = 0;
        while (i_7 < m.rows) {
            newData_1 = ((double[][])(appendObj(newData_1, java.util.stream.DoubleStream.concat(java.util.Arrays.stream(m.data[i_7]), java.util.stream.DoubleStream.of(col[i_7])).toArray())));
            i_7 = i_7 + 1;
        }
        return new Matrix(newData_1, m.rows, m.cols + 1);
    }

    static Matrix matrix_mul_scalar(Matrix m, double s) {
        double[][] vals_5 = ((double[][])(new double[][]{}));
        int i_8 = 0;
        while (i_8 < m.rows) {
            double[] row_5 = ((double[])(new double[]{}));
            int j_7 = 0;
            while (j_7 < m.cols) {
                row_5 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row_5), java.util.stream.DoubleStream.of(m.data[i_8][j_7] * s)).toArray()));
                j_7 = j_7 + 1;
            }
            vals_5 = ((double[][])(appendObj(vals_5, row_5)));
            i_8 = i_8 + 1;
        }
        return new Matrix(vals_5, m.rows, m.cols);
    }

    static Matrix matrix_neg(Matrix m) {
        return matrix_mul_scalar(m, -1.0);
    }

    static Matrix matrix_add(Matrix a, Matrix b) {
        if (a.rows != b.rows || a.cols != b.cols) {
            return new Matrix(new double[][]{}, 0, 0);
        }
        double[][] vals_6 = ((double[][])(new double[][]{}));
        int i_9 = 0;
        while (i_9 < a.rows) {
            double[] row_6 = ((double[])(new double[]{}));
            int j_8 = 0;
            while (j_8 < a.cols) {
                row_6 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row_6), java.util.stream.DoubleStream.of(a.data[i_9][j_8] + b.data[i_9][j_8])).toArray()));
                j_8 = j_8 + 1;
            }
            vals_6 = ((double[][])(appendObj(vals_6, row_6)));
            i_9 = i_9 + 1;
        }
        return new Matrix(vals_6, a.rows, a.cols);
    }

    static Matrix matrix_sub(Matrix a, Matrix b) {
        if (a.rows != b.rows || a.cols != b.cols) {
            return new Matrix(new double[][]{}, 0, 0);
        }
        double[][] vals_7 = ((double[][])(new double[][]{}));
        int i_10 = 0;
        while (i_10 < a.rows) {
            double[] row_7 = ((double[])(new double[]{}));
            int j_9 = 0;
            while (j_9 < a.cols) {
                row_7 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row_7), java.util.stream.DoubleStream.of(a.data[i_10][j_9] - b.data[i_10][j_9])).toArray()));
                j_9 = j_9 + 1;
            }
            vals_7 = ((double[][])(appendObj(vals_7, row_7)));
            i_10 = i_10 + 1;
        }
        return new Matrix(vals_7, a.rows, a.cols);
    }

    static double matrix_dot(double[] row, double[] col) {
        double sum_1 = 0.0;
        int i_11 = 0;
        while (i_11 < row.length) {
            sum_1 = sum_1 + row[i_11] * col[i_11];
            i_11 = i_11 + 1;
        }
        return sum_1;
    }

    static Matrix matrix_mul(Matrix a, Matrix b) {
        if (a.cols != b.rows) {
            return new Matrix(new double[][]{}, 0, 0);
        }
        double[][] bcols = ((double[][])(matrix_columns(b)));
        double[][] vals_8 = ((double[][])(new double[][]{}));
        int i_12 = 0;
        while (i_12 < a.rows) {
            double[] row_8 = ((double[])(new double[]{}));
            int j_10 = 0;
            while (j_10 < b.cols) {
                row_8 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row_8), java.util.stream.DoubleStream.of(matrix_dot(((double[])(a.data[i_12])), ((double[])(bcols[j_10]))))).toArray()));
                j_10 = j_10 + 1;
            }
            vals_8 = ((double[][])(appendObj(vals_8, row_8)));
            i_12 = i_12 + 1;
        }
        return new Matrix(vals_8, a.rows, b.cols);
    }

    static Matrix matrix_pow(Matrix m, int p) {
        if (p == 0) {
            return matrix_identity(m);
        }
        if (p < 0) {
            if (((Boolean)(matrix_is_invertible(m)))) {
                return matrix_pow(matrix_inverse(m), -p);
            }
            return new Matrix(new double[][]{}, 0, 0);
        }
        Matrix result = m;
        int i_13 = 1;
        while (i_13 < p) {
            result = matrix_mul(result, m);
            i_13 = i_13 + 1;
        }
        return result;
    }

    static String matrix_to_string(Matrix m) {
        if (m.rows == 0) {
            return "[]";
        }
        String s = "[";
        int i_14 = 0;
        while (i_14 < m.rows) {
            s = s + "[";
            int j_11 = 0;
            while (j_11 < m.cols) {
                s = s + _p(_getd(m.data[i_14], j_11));
                if (j_11 < m.cols - 1) {
                    s = s + " ";
                }
                j_11 = j_11 + 1;
            }
            s = s + "]";
            if (i_14 < m.rows - 1) {
                s = s + "\n ";
            }
            i_14 = i_14 + 1;
        }
        s = s + "]";
        return s;
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
        Matrix m2 = matrix_mul_scalar(m, 3.0);
        System.out.println(matrix_to_string(m2));
        System.out.println(matrix_to_string(matrix_add(m, m2)));
        System.out.println(matrix_to_string(matrix_sub(m, m2)));
        System.out.println(matrix_to_string(matrix_pow(m, 3)));
        Matrix m3 = matrix_add_row(m, ((double[])(new double[]{10.0, 11.0, 12.0})));
        System.out.println(matrix_to_string(m3));
        Matrix m4 = matrix_add_column(m2, ((double[])(new double[]{8.0, 16.0, 32.0})));
        System.out.println(matrix_to_string(matrix_mul(m3, m4)));
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

    static Double _getd(double[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
