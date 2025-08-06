public class Main {

    static double floor(double x) {
        int i = ((Number)(x)).intValue();
        if ((((Number)(i)).doubleValue()) > x) {
            i = i - 1;
        }
        return ((Number)(i)).doubleValue();
    }

    static double pow10(int n) {
        double p = 1.0;
        int i_1 = 0;
        while (i_1 < n) {
            p = p * 10.0;
            i_1 = i_1 + 1;
        }
        return p;
    }

    static double round(double x, int n) {
        double m = pow10(n);
        return floor(x * m + 0.5) / m;
    }

    static double[][] clone_matrix(double[][] mat) {
        double[][] new_mat = ((double[][])(new double[][]{}));
        int i_2 = 0;
        while (i_2 < mat.length) {
            double[] row = ((double[])(new double[]{}));
            int j = 0;
            while (j < mat[i_2].length) {
                row = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row), java.util.stream.DoubleStream.of(mat[i_2][j])).toArray()));
                j = j + 1;
            }
            new_mat = ((double[][])(appendObj(new_mat, row)));
            i_2 = i_2 + 1;
        }
        return new_mat;
    }

    static double[] solve_simultaneous(double[][] equations) {
        int n = equations.length;
        if (n == 0) {
            throw new RuntimeException(String.valueOf("solve_simultaneous() requires n lists of length n+1"));
        }
        int m_1 = n + 1;
        int i_3 = 0;
        while (i_3 < n) {
            if (equations[i_3].length != m_1) {
                throw new RuntimeException(String.valueOf("solve_simultaneous() requires n lists of length n+1"));
            }
            i_3 = i_3 + 1;
        }
        double[][] a = ((double[][])(clone_matrix(((double[][])(equations)))));
        int row_1 = 0;
        while (row_1 < n) {
            int pivot = row_1;
            while (pivot < n && a[pivot][row_1] == 0.0) {
                pivot = pivot + 1;
            }
            if (pivot == n) {
                throw new RuntimeException(String.valueOf("solve_simultaneous() requires at least 1 full equation"));
            }
            if (pivot != row_1) {
                double[] temp = ((double[])(a[row_1]));
a[row_1] = ((double[])(a[pivot]));
a[pivot] = ((double[])(temp));
            }
            double pivot_val = a[row_1][row_1];
            int col = 0;
            while (col < m_1) {
a[row_1][col] = a[row_1][col] / pivot_val;
                col = col + 1;
            }
            int r = 0;
            while (r < n) {
                if (r != row_1) {
                    double factor = a[r][row_1];
                    int c = 0;
                    while (c < m_1) {
a[r][c] = a[r][c] - factor * a[row_1][c];
                        c = c + 1;
                    }
                }
                r = r + 1;
            }
            row_1 = row_1 + 1;
        }
        double[] res = ((double[])(new double[]{}));
        int k = 0;
        while (k < n) {
            res = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(res), java.util.stream.DoubleStream.of(round(a[k][m_1 - 1], 5))).toArray()));
            k = k + 1;
        }
        return res;
    }

    static void test_solver() {
        double[][] a_1 = ((double[][])(new double[][]{new double[]{1.0, 2.0, 3.0}, new double[]{4.0, 5.0, 6.0}}));
        double[] r1 = ((double[])(solve_simultaneous(((double[][])(a_1)))));
        if (!(r1.length == 2 && r1[0] == (0.0 - 1.0) && r1[1] == 2.0)) {
            throw new RuntimeException(String.valueOf("test1 failed"));
        }
        double[][] b = ((double[][])(new double[][]{new double[]{0.0, (0.0 - 3.0), 1.0, 7.0}, new double[]{3.0, 2.0, (0.0 - 1.0), 11.0}, new double[]{5.0, 1.0, (0.0 - 2.0), 12.0}}));
        double[] r2 = ((double[])(solve_simultaneous(((double[][])(b)))));
        if (!(r2.length == 3 && r2[0] == 6.4 && r2[1] == 1.2 && r2[2] == 10.6)) {
            throw new RuntimeException(String.valueOf("test2 failed"));
        }
    }

    static void main() {
        test_solver();
        double[][] eq = ((double[][])(new double[][]{new double[]{2.0, 1.0, 1.0, 1.0, 1.0, 4.0}, new double[]{1.0, 2.0, 1.0, 1.0, 1.0, 5.0}, new double[]{1.0, 1.0, 2.0, 1.0, 1.0, 6.0}, new double[]{1.0, 1.0, 1.0, 2.0, 1.0, 7.0}, new double[]{1.0, 1.0, 1.0, 1.0, 2.0, 8.0}}));
        System.out.println(_p(solve_simultaneous(((double[][])(eq)))));
        System.out.println(_p(solve_simultaneous(((double[][])(new double[][]{new double[]{4.0, 2.0}})))));
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
