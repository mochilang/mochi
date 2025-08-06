public class Main {
    static class OptionMatrix {
        double[][] value;
        boolean ok;
        OptionMatrix(double[][] value, boolean ok) {
            this.value = value;
            this.ok = ok;
        }
        OptionMatrix() {}
        @Override public String toString() {
            return String.format("{'value': %s, 'ok': %s}", String.valueOf(value), String.valueOf(ok));
        }
    }


    static double[][] identity(int n) {
        double[][] mat = ((double[][])(new double[][]{}));
        int i = 0;
        while (i < n) {
            double[] row = ((double[])(new double[]{}));
            int j = 0;
            while (j < n) {
                if (i == j) {
                    row = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row), java.util.stream.DoubleStream.of(1.0)).toArray()));
                } else {
                    row = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row), java.util.stream.DoubleStream.of(0.0)).toArray()));
                }
                j = j + 1;
            }
            mat = ((double[][])(appendObj(mat, row)));
            i = i + 1;
        }
        return mat;
    }

    static double[][] transpose(double[][] mat) {
        int rows = mat.length;
        int cols = mat[0].length;
        double[][] res = ((double[][])(new double[][]{}));
        int j_1 = 0;
        while (j_1 < cols) {
            double[] row_1 = ((double[])(new double[]{}));
            int i_1 = 0;
            while (i_1 < rows) {
                row_1 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row_1), java.util.stream.DoubleStream.of(mat[i_1][j_1])).toArray()));
                i_1 = i_1 + 1;
            }
            res = ((double[][])(appendObj(res, row_1)));
            j_1 = j_1 + 1;
        }
        return res;
    }

    static double[][] matmul(double[][] a, double[][] b) {
        int rows_1 = a.length;
        int cols_1 = b[0].length;
        int inner = a[0].length;
        double[][] res_1 = ((double[][])(new double[][]{}));
        int i_2 = 0;
        while (i_2 < rows_1) {
            double[] row_2 = ((double[])(new double[]{}));
            int j_2 = 0;
            while (j_2 < cols_1) {
                double sum = 0.0;
                int k = 0;
                while (k < inner) {
                    sum = sum + a[i_2][k] * b[k][j_2];
                    k = k + 1;
                }
                row_2 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row_2), java.util.stream.DoubleStream.of(sum)).toArray()));
                j_2 = j_2 + 1;
            }
            res_1 = ((double[][])(appendObj(res_1, row_2)));
            i_2 = i_2 + 1;
        }
        return res_1;
    }

    static double[][] mat_sub(double[][] a, double[][] b) {
        int rows_2 = a.length;
        int cols_2 = a[0].length;
        double[][] res_2 = ((double[][])(new double[][]{}));
        int i_3 = 0;
        while (i_3 < rows_2) {
            double[] row_3 = ((double[])(new double[]{}));
            int j_3 = 0;
            while (j_3 < cols_2) {
                row_3 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row_3), java.util.stream.DoubleStream.of(a[i_3][j_3] - b[i_3][j_3])).toArray()));
                j_3 = j_3 + 1;
            }
            res_2 = ((double[][])(appendObj(res_2, row_3)));
            i_3 = i_3 + 1;
        }
        return res_2;
    }

    static double[][] inverse(double[][] mat) {
        int n = mat.length;
        double[][] id = ((double[][])(identity(n)));
        double[][] aug = ((double[][])(new double[][]{}));
        int i_4 = 0;
        while (i_4 < n) {
            Object row_4 = concat(mat[i_4], id[i_4]);
            aug = ((double[][])(appendObj(aug, row_4)));
            i_4 = i_4 + 1;
        }
        int col = 0;
        while (col < n) {
            double[] pivot_row = ((double[])(aug[col]));
            double pivot = pivot_row[col];
            if (pivot == 0.0) {
                throw new RuntimeException(String.valueOf("matrix is singular"));
            }
            int j_4 = 0;
            while (j_4 < 2 * n) {
pivot_row[j_4] = pivot_row[j_4] / pivot;
                j_4 = j_4 + 1;
            }
aug[col] = ((double[])(pivot_row));
            int r = 0;
            while (r < n) {
                if (r != col) {
                    double[] row_r = ((double[])(aug[r]));
                    double factor = row_r[col];
                    j_4 = 0;
                    while (j_4 < 2 * n) {
row_r[j_4] = row_r[j_4] - factor * pivot_row[j_4];
                        j_4 = j_4 + 1;
                    }
aug[r] = ((double[])(row_r));
                }
                r = r + 1;
            }
            col = col + 1;
        }
        double[][] inv = ((double[][])(new double[][]{}));
        int r_1 = 0;
        while (r_1 < n) {
            double[] row_5 = ((double[])(new double[]{}));
            int c = n;
            while (c < 2 * n) {
                row_5 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row_5), java.util.stream.DoubleStream.of(aug[r_1][c])).toArray()));
                c = c + 1;
            }
            inv = ((double[][])(appendObj(inv, row_5)));
            r_1 = r_1 + 1;
        }
        return inv;
    }

    static double[][] schur_complement(double[][] mat_a, double[][] mat_b, double[][] mat_c, OptionMatrix pseudo_inv) {
        int a_rows = mat_a.length;
        int a_cols = mat_a[0].length;
        if (a_rows != a_cols) {
            throw new RuntimeException(String.valueOf("Matrix A must be square"));
        }
        if (a_rows != mat_b.length) {
            throw new RuntimeException(String.valueOf("Expected the same number of rows for A and B"));
        }
        if (mat_b[0].length != mat_c[0].length) {
            throw new RuntimeException(String.valueOf("Expected the same number of columns for B and C"));
        }
        double[][] a_inv = new double[0][];
        if (pseudo_inv.ok) {
            a_inv = ((double[][])(pseudo_inv.value));
        } else {
            a_inv = ((double[][])(inverse(((double[][])(mat_a)))));
        }
        double[][] bt = ((double[][])(transpose(((double[][])(mat_b)))));
        double[][] a_inv_b = ((double[][])(matmul(((double[][])(a_inv)), ((double[][])(mat_b)))));
        double[][] bt_a_inv_b = ((double[][])(matmul(((double[][])(bt)), ((double[][])(a_inv_b)))));
        return mat_sub(((double[][])(mat_c)), ((double[][])(bt_a_inv_b)));
    }

    static void print_matrix(double[][] mat) {
        int i_5 = 0;
        while (i_5 < mat.length) {
            String line = "";
            int j_5 = 0;
            double[] row_6 = ((double[])(mat[i_5]));
            while (j_5 < row_6.length) {
                line = line + _p(_geto(row_6, j_5));
                if (j_5 + 1 < row_6.length) {
                    line = line + " ";
                }
                j_5 = j_5 + 1;
            }
            System.out.println(line);
            i_5 = i_5 + 1;
        }
    }

    static void main() {
        double[][] a = ((double[][])(new double[][]{new double[]{1.0, 2.0}, new double[]{2.0, 1.0}}));
        double[][] b = ((double[][])(new double[][]{new double[]{0.0, 3.0}, new double[]{3.0, 0.0}}));
        double[][] c_1 = ((double[][])(new double[][]{new double[]{2.0, 1.0}, new double[]{6.0, 3.0}}));
        OptionMatrix none = new OptionMatrix(new double[][]{}, false);
        double[][] s = ((double[][])(schur_complement(((double[][])(a)), ((double[][])(b)), ((double[][])(c_1)), none)));
        print_matrix(((double[][])(s)));
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

    static <T> T[] concat(T[] a, T[] b) {
        T[] out = java.util.Arrays.copyOf(a, a.length + b.length);
        System.arraycopy(b, 0, out, a.length, b.length);
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

    static Object _geto(Object[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
