public class Main {

    static double[][] add(double[][][] matrices) {
        int rows = matrices[0].length;
        int cols = matrices[0][0].length;
        int r = 0;
        double[][] result = ((double[][])(new double[][]{}));
        while (r < rows) {
            double[] row = ((double[])(new double[]{}));
            int c = 0;
            while (c < cols) {
                double sum = 0.0;
                int m = 0;
                while (m < matrices.length) {
                    sum = sum + matrices[m][r][c];
                    m = m + 1;
                }
                row = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row), java.util.stream.DoubleStream.of(sum)).toArray()));
                c = c + 1;
            }
            result = ((double[][])(appendObj(result, row)));
            r = r + 1;
        }
        return result;
    }

    static double[][] subtract(double[][] a, double[][] b) {
        int rows_1 = a.length;
        int cols_1 = a[0].length;
        int r_1 = 0;
        double[][] result_1 = ((double[][])(new double[][]{}));
        while (r_1 < rows_1) {
            double[] row_1 = ((double[])(new double[]{}));
            int c_1 = 0;
            while (c_1 < cols_1) {
                row_1 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row_1), java.util.stream.DoubleStream.of(a[r_1][c_1] - b[r_1][c_1])).toArray()));
                c_1 = c_1 + 1;
            }
            result_1 = ((double[][])(appendObj(result_1, row_1)));
            r_1 = r_1 + 1;
        }
        return result_1;
    }

    static double[][] scalar_multiply(double[][] matrix, double n) {
        double[][] result_2 = ((double[][])(new double[][]{}));
        int i = 0;
        while (i < matrix.length) {
            double[] row_2 = ((double[])(new double[]{}));
            int j = 0;
            while (j < matrix[i].length) {
                row_2 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row_2), java.util.stream.DoubleStream.of(matrix[i][j] * n)).toArray()));
                j = j + 1;
            }
            result_2 = ((double[][])(appendObj(result_2, row_2)));
            i = i + 1;
        }
        return result_2;
    }

    static double[][] multiply(double[][] a, double[][] b) {
        int rowsA = a.length;
        int colsA = a[0].length;
        int rowsB = b.length;
        int colsB = b[0].length;
        double[][] result_3 = ((double[][])(new double[][]{}));
        int i_1 = 0;
        while (i_1 < rowsA) {
            double[] row_3 = ((double[])(new double[]{}));
            int j_1 = 0;
            while (j_1 < colsB) {
                double sum_1 = 0.0;
                int k = 0;
                while (k < colsA) {
                    sum_1 = sum_1 + a[i_1][k] * b[k][j_1];
                    k = k + 1;
                }
                row_3 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row_3), java.util.stream.DoubleStream.of(sum_1)).toArray()));
                j_1 = j_1 + 1;
            }
            result_3 = ((double[][])(appendObj(result_3, row_3)));
            i_1 = i_1 + 1;
        }
        return result_3;
    }

    static double[][] identity(int n) {
        double[][] result_4 = ((double[][])(new double[][]{}));
        int i_2 = 0;
        while (i_2 < n) {
            double[] row_4 = ((double[])(new double[]{}));
            int j_2 = 0;
            while (j_2 < n) {
                if (i_2 == j_2) {
                    row_4 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row_4), java.util.stream.DoubleStream.of(1.0)).toArray()));
                } else {
                    row_4 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row_4), java.util.stream.DoubleStream.of(0.0)).toArray()));
                }
                j_2 = j_2 + 1;
            }
            result_4 = ((double[][])(appendObj(result_4, row_4)));
            i_2 = i_2 + 1;
        }
        return result_4;
    }

    static double[][] transpose(double[][] matrix) {
        int rows_2 = matrix.length;
        int cols_2 = matrix[0].length;
        double[][] result_5 = ((double[][])(new double[][]{}));
        int c_2 = 0;
        while (c_2 < cols_2) {
            double[] row_5 = ((double[])(new double[]{}));
            int r_2 = 0;
            while (r_2 < rows_2) {
                row_5 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row_5), java.util.stream.DoubleStream.of(matrix[r_2][c_2])).toArray()));
                r_2 = r_2 + 1;
            }
            result_5 = ((double[][])(appendObj(result_5, row_5)));
            c_2 = c_2 + 1;
        }
        return result_5;
    }

    static double[][] minor(double[][] matrix, int row, int column) {
        double[][] result_6 = ((double[][])(new double[][]{}));
        int i_3 = 0;
        while (i_3 < matrix.length) {
            if (i_3 != row) {
                double[] new_row = ((double[])(new double[]{}));
                int j_3 = 0;
                while (j_3 < matrix[i_3].length) {
                    if (j_3 != column) {
                        new_row = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(new_row), java.util.stream.DoubleStream.of(matrix[i_3][j_3])).toArray()));
                    }
                    j_3 = j_3 + 1;
                }
                result_6 = ((double[][])(appendObj(result_6, new_row)));
            }
            i_3 = i_3 + 1;
        }
        return result_6;
    }

    static double determinant(double[][] matrix) {
        if (matrix.length == 1) {
            return matrix[0][0];
        }
        double det = 0.0;
        int c_3 = 0;
        while (c_3 < matrix[0].length) {
            double[][] sub = ((double[][])(minor(((double[][])(matrix)), 0, c_3)));
            double sign = Math.floorMod(c_3, 2) == 0 ? 1.0 : -1.0;
            det = det + matrix[0][c_3] * determinant(((double[][])(sub))) * sign;
            c_3 = c_3 + 1;
        }
        return det;
    }

    static double[][] inverse(double[][] matrix) {
        double det_1 = determinant(((double[][])(matrix)));
        if (det_1 == 0.0) {
            return new double[][]{};
        }
        int size = matrix.length;
        double[][] matrix_minor = ((double[][])(new double[][]{}));
        int i_4 = 0;
        while (i_4 < size) {
            double[] row_6 = ((double[])(new double[]{}));
            int j_4 = 0;
            while (j_4 < size) {
                double[][] m_1 = ((double[][])(minor(((double[][])(matrix)), i_4, j_4)));
                row_6 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row_6), java.util.stream.DoubleStream.of(determinant(((double[][])(m_1))))).toArray()));
                j_4 = j_4 + 1;
            }
            matrix_minor = ((double[][])(appendObj(matrix_minor, row_6)));
            i_4 = i_4 + 1;
        }
        double[][] cofactors = ((double[][])(new double[][]{}));
        i_4 = 0;
        while (i_4 < size) {
            double[] row_7 = ((double[])(new double[]{}));
            int j_5 = 0;
            while (j_5 < size) {
                double sign_1 = Math.floorMod((i_4 + j_5), 2) == 0 ? 1.0 : -1.0;
                row_7 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row_7), java.util.stream.DoubleStream.of(matrix_minor[i_4][j_5] * sign_1)).toArray()));
                j_5 = j_5 + 1;
            }
            cofactors = ((double[][])(appendObj(cofactors, row_7)));
            i_4 = i_4 + 1;
        }
        double[][] adjugate = ((double[][])(transpose(((double[][])(cofactors)))));
        return scalar_multiply(((double[][])(adjugate)), 1.0 / det_1);
    }

    static void main() {
        double[][] matrix_a = ((double[][])(new double[][]{new double[]{12.0, 10.0}, new double[]{3.0, 9.0}}));
        double[][] matrix_b = ((double[][])(new double[][]{new double[]{3.0, 4.0}, new double[]{7.0, 4.0}}));
        double[][] matrix_c = ((double[][])(new double[][]{new double[]{11.0, 12.0, 13.0, 14.0}, new double[]{21.0, 22.0, 23.0, 24.0}, new double[]{31.0, 32.0, 33.0, 34.0}, new double[]{41.0, 42.0, 43.0, 44.0}}));
        double[][] matrix_d = ((double[][])(new double[][]{new double[]{3.0, 0.0, 2.0}, new double[]{2.0, 0.0, -2.0}, new double[]{0.0, 1.0, 1.0}}));
        System.out.println("Add Operation, add(matrix_a, matrix_b) = " + _p(add(((double[][][])(new double[][][]{matrix_a, matrix_b})))) + " \n");
        System.out.println("Multiply Operation, multiply(matrix_a, matrix_b) = " + _p(multiply(((double[][])(matrix_a)), ((double[][])(matrix_b)))) + " \n");
        System.out.println("Identity: " + _p(identity(5)) + "\n");
        System.out.println("Minor of " + _p(matrix_c) + " = " + _p(minor(((double[][])(matrix_c)), 1, 2)) + " \n");
        System.out.println("Determinant of " + _p(matrix_b) + " = " + _p(determinant(((double[][])(matrix_b)))) + " \n");
        System.out.println("Inverse of " + _p(matrix_d) + " = " + _p(inverse(((double[][])(matrix_d)))) + "\n");
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
}
