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


    static Matrix make_matrix(int rows, int cols, double value) {
        double[][] arr = ((double[][])(new double[][]{}));
        int r = 0;
        while (r < rows) {
            double[] row = ((double[])(new double[]{}));
            int c = 0;
            while (c < cols) {
                row = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row), java.util.stream.DoubleStream.of(value)).toArray()));
                c = c + 1;
            }
            arr = ((double[][])(appendObj(arr, row)));
            r = r + 1;
        }
        return new Matrix(arr, rows, cols);
    }

    static Matrix matrix_from_lists(double[][] vals) {
        int r_1 = vals.length;
        int c_1 = r_1 == 0 ? 0 : vals[0].length;
        return new Matrix(vals, r_1, c_1);
    }

    static String matrix_to_string(Matrix m) {
        String s = "";
        int i = 0;
        while (i < m.rows) {
            s = s + "[";
            int j = 0;
            while (j < m.cols) {
                s = s + _p(_getd(m.data[i], j));
                if (j < m.cols - 1) {
                    s = s + ", ";
                }
                j = j + 1;
            }
            s = s + "]";
            if (i < m.rows - 1) {
                s = s + "\n";
            }
            i = i + 1;
        }
        return s;
    }

    static Matrix matrix_add(Matrix a, Matrix b) {
        if (a.rows != b.rows || a.cols != b.cols) {
            return new Matrix(new double[][]{}, 0, 0);
        }
        double[][] res = ((double[][])(new double[][]{}));
        int i_1 = 0;
        while (i_1 < a.rows) {
            double[] row_1 = ((double[])(new double[]{}));
            int j_1 = 0;
            while (j_1 < a.cols) {
                row_1 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row_1), java.util.stream.DoubleStream.of(a.data[i_1][j_1] + b.data[i_1][j_1])).toArray()));
                j_1 = j_1 + 1;
            }
            res = ((double[][])(appendObj(res, row_1)));
            i_1 = i_1 + 1;
        }
        return new Matrix(res, a.rows, a.cols);
    }

    static Matrix matrix_sub(Matrix a, Matrix b) {
        if (a.rows != b.rows || a.cols != b.cols) {
            return new Matrix(new double[][]{}, 0, 0);
        }
        double[][] res_1 = ((double[][])(new double[][]{}));
        int i_2 = 0;
        while (i_2 < a.rows) {
            double[] row_2 = ((double[])(new double[]{}));
            int j_2 = 0;
            while (j_2 < a.cols) {
                row_2 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row_2), java.util.stream.DoubleStream.of(a.data[i_2][j_2] - b.data[i_2][j_2])).toArray()));
                j_2 = j_2 + 1;
            }
            res_1 = ((double[][])(appendObj(res_1, row_2)));
            i_2 = i_2 + 1;
        }
        return new Matrix(res_1, a.rows, a.cols);
    }

    static Matrix matrix_mul_scalar(Matrix m, double k) {
        double[][] res_2 = ((double[][])(new double[][]{}));
        int i_3 = 0;
        while (i_3 < m.rows) {
            double[] row_3 = ((double[])(new double[]{}));
            int j_3 = 0;
            while (j_3 < m.cols) {
                row_3 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row_3), java.util.stream.DoubleStream.of(m.data[i_3][j_3] * k)).toArray()));
                j_3 = j_3 + 1;
            }
            res_2 = ((double[][])(appendObj(res_2, row_3)));
            i_3 = i_3 + 1;
        }
        return new Matrix(res_2, m.rows, m.cols);
    }

    static Matrix matrix_mul(Matrix a, Matrix b) {
        if (a.cols != b.rows) {
            return new Matrix(new double[][]{}, 0, 0);
        }
        double[][] res_3 = ((double[][])(new double[][]{}));
        int i_4 = 0;
        while (i_4 < a.rows) {
            double[] row_4 = ((double[])(new double[]{}));
            int j_4 = 0;
            while (j_4 < b.cols) {
                double sum = 0.0;
                int k = 0;
                while (k < a.cols) {
                    sum = sum + a.data[i_4][k] * b.data[k][j_4];
                    k = k + 1;
                }
                row_4 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row_4), java.util.stream.DoubleStream.of(sum)).toArray()));
                j_4 = j_4 + 1;
            }
            res_3 = ((double[][])(appendObj(res_3, row_4)));
            i_4 = i_4 + 1;
        }
        return new Matrix(res_3, a.rows, b.cols);
    }

    static Matrix matrix_transpose(Matrix m) {
        double[][] res_4 = ((double[][])(new double[][]{}));
        int c_2 = 0;
        while (c_2 < m.cols) {
            double[] row_5 = ((double[])(new double[]{}));
            int r_2 = 0;
            while (r_2 < m.rows) {
                row_5 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row_5), java.util.stream.DoubleStream.of(m.data[r_2][c_2])).toArray()));
                r_2 = r_2 + 1;
            }
            res_4 = ((double[][])(appendObj(res_4, row_5)));
            c_2 = c_2 + 1;
        }
        return new Matrix(res_4, m.cols, m.rows);
    }

    static Matrix sherman_morrison(Matrix ainv, Matrix u, Matrix v) {
        Matrix vt = matrix_transpose(v);
        Matrix vu = matrix_mul(matrix_mul(vt, ainv), u);
        double factor = vu.data[0][0] + 1.0;
        if (factor == 0.0) {
            return new Matrix(new double[][]{}, 0, 0);
        }
        Matrix term1 = matrix_mul(ainv, u);
        Matrix term2 = matrix_mul(vt, ainv);
        Matrix numerator = matrix_mul(term1, term2);
        Matrix scaled = matrix_mul_scalar(numerator, 1.0 / factor);
        return matrix_sub(ainv, scaled);
    }

    static void main() {
        Matrix ainv = matrix_from_lists(((double[][])(new double[][]{new double[]{1.0, 0.0, 0.0}, new double[]{0.0, 1.0, 0.0}, new double[]{0.0, 0.0, 1.0}})));
        Matrix u = matrix_from_lists(((double[][])(new double[][]{new double[]{1.0}, new double[]{2.0}, new double[]{-3.0}})));
        Matrix v = matrix_from_lists(((double[][])(new double[][]{new double[]{4.0}, new double[]{-2.0}, new double[]{5.0}})));
        Matrix result = sherman_morrison(ainv, u, v);
        System.out.println(matrix_to_string(result));
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
