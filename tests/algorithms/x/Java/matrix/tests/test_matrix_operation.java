public class Main {

    static void check_matrix(double[][] mat) {
        if (mat.length < 2 || mat[0].length < 2) {
            throw new RuntimeException(String.valueOf("Expected a matrix with at least 2x2 dimensions"));
        }
    }

    static double[][] add(double[][] a, double[][] b) {
        check_matrix(((double[][])(a)));
        check_matrix(((double[][])(b)));
        if (a.length != b.length || a[0].length != b[0].length) {
            throw new RuntimeException(String.valueOf("Matrices must have the same dimensions"));
        }
        int rows = a.length;
        int cols = a[0].length;
        double[][] result = ((double[][])(new double[][]{}));
        int i = 0;
        while (i < rows) {
            double[] row = ((double[])(new double[]{}));
            int j = 0;
            while (j < cols) {
                row = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row), java.util.stream.DoubleStream.of(a[i][j] + b[i][j])).toArray()));
                j = j + 1;
            }
            result = ((double[][])(appendObj(result, row)));
            i = i + 1;
        }
        return result;
    }

    static double[][] subtract(double[][] a, double[][] b) {
        check_matrix(((double[][])(a)));
        check_matrix(((double[][])(b)));
        if (a.length != b.length || a[0].length != b[0].length) {
            throw new RuntimeException(String.valueOf("Matrices must have the same dimensions"));
        }
        int rows_1 = a.length;
        int cols_1 = a[0].length;
        double[][] result_1 = ((double[][])(new double[][]{}));
        int i_1 = 0;
        while (i_1 < rows_1) {
            double[] row_1 = ((double[])(new double[]{}));
            int j_1 = 0;
            while (j_1 < cols_1) {
                row_1 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row_1), java.util.stream.DoubleStream.of(a[i_1][j_1] - b[i_1][j_1])).toArray()));
                j_1 = j_1 + 1;
            }
            result_1 = ((double[][])(appendObj(result_1, row_1)));
            i_1 = i_1 + 1;
        }
        return result_1;
    }

    static double[][] scalar_multiply(double[][] a, double s) {
        check_matrix(((double[][])(a)));
        int rows_2 = a.length;
        int cols_2 = a[0].length;
        double[][] result_2 = ((double[][])(new double[][]{}));
        int i_2 = 0;
        while (i_2 < rows_2) {
            double[] row_2 = ((double[])(new double[]{}));
            int j_2 = 0;
            while (j_2 < cols_2) {
                row_2 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row_2), java.util.stream.DoubleStream.of(a[i_2][j_2] * s)).toArray()));
                j_2 = j_2 + 1;
            }
            result_2 = ((double[][])(appendObj(result_2, row_2)));
            i_2 = i_2 + 1;
        }
        return result_2;
    }

    static double[][] multiply(double[][] a, double[][] b) {
        check_matrix(((double[][])(a)));
        check_matrix(((double[][])(b)));
        if (a[0].length != b.length) {
            throw new RuntimeException(String.valueOf("Invalid dimensions for matrix multiplication"));
        }
        int rows_3 = a.length;
        int cols_3 = b[0].length;
        double[][] result_3 = ((double[][])(new double[][]{}));
        int i_3 = 0;
        while (i_3 < rows_3) {
            double[] row_3 = ((double[])(new double[]{}));
            int j_3 = 0;
            while (j_3 < cols_3) {
                double sum = 0.0;
                int k = 0;
                while (k < b.length) {
                    sum = sum + a[i_3][k] * b[k][j_3];
                    k = k + 1;
                }
                row_3 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row_3), java.util.stream.DoubleStream.of(sum)).toArray()));
                j_3 = j_3 + 1;
            }
            result_3 = ((double[][])(appendObj(result_3, row_3)));
            i_3 = i_3 + 1;
        }
        return result_3;
    }

    static double[][] identity(int n) {
        double[][] result_4 = ((double[][])(new double[][]{}));
        int i_4 = 0;
        while (i_4 < n) {
            double[] row_4 = ((double[])(new double[]{}));
            int j_4 = 0;
            while (j_4 < n) {
                if (i_4 == j_4) {
                    row_4 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row_4), java.util.stream.DoubleStream.of(1.0)).toArray()));
                } else {
                    row_4 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row_4), java.util.stream.DoubleStream.of(0.0)).toArray()));
                }
                j_4 = j_4 + 1;
            }
            result_4 = ((double[][])(appendObj(result_4, row_4)));
            i_4 = i_4 + 1;
        }
        return result_4;
    }

    static double[][] transpose(double[][] a) {
        check_matrix(((double[][])(a)));
        int rows_4 = a.length;
        int cols_4 = a[0].length;
        double[][] result_5 = ((double[][])(new double[][]{}));
        int j_5 = 0;
        while (j_5 < cols_4) {
            double[] row_5 = ((double[])(new double[]{}));
            int i_5 = 0;
            while (i_5 < rows_4) {
                row_5 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row_5), java.util.stream.DoubleStream.of(a[i_5][j_5])).toArray()));
                i_5 = i_5 + 1;
            }
            result_5 = ((double[][])(appendObj(result_5, row_5)));
            j_5 = j_5 + 1;
        }
        return result_5;
    }

    static void main() {
        double[][] mat_a = ((double[][])(new double[][]{new double[]{12.0, 10.0}, new double[]{3.0, 9.0}}));
        double[][] mat_b = ((double[][])(new double[][]{new double[]{3.0, 4.0}, new double[]{7.0, 4.0}}));
        double[][] mat_c = ((double[][])(new double[][]{new double[]{3.0, 0.0, 2.0}, new double[]{2.0, 0.0, -2.0}, new double[]{0.0, 1.0, 1.0}}));
        System.out.println(_p(add(((double[][])(mat_a)), ((double[][])(mat_b)))));
        System.out.println(_p(subtract(((double[][])(mat_a)), ((double[][])(mat_b)))));
        System.out.println(_p(multiply(((double[][])(mat_a)), ((double[][])(mat_b)))));
        System.out.println(_p(scalar_multiply(((double[][])(mat_a)), 3.5)));
        System.out.println(_p(identity(5)));
        System.out.println(_p(transpose(((double[][])(mat_c)))));
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
