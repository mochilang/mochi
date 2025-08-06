public class Main {

    static int[][] identity(int n) {
        int i = 0;
        int[][] mat = ((int[][])(new int[][]{}));
        while (i < n) {
            int[] row = ((int[])(new int[]{}));
            int j = 0;
            while (j < n) {
                if (i == j) {
                    row = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(row), java.util.stream.IntStream.of(1)).toArray()));
                } else {
                    row = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(row), java.util.stream.IntStream.of(0)).toArray()));
                }
                j = j + 1;
            }
            mat = ((int[][])(appendObj(mat, row)));
            i = i + 1;
        }
        return mat;
    }

    static int[][] matrix_mul(int[][] a, int[][] b) {
        int n = a.length;
        int[][] result = ((int[][])(new int[][]{}));
        int i_1 = 0;
        while (i_1 < n) {
            int[] row_1 = ((int[])(new int[]{}));
            int j_1 = 0;
            while (j_1 < n) {
                int cell = 0;
                int k = 0;
                while (k < n) {
                    cell = cell + a[i_1][k] * b[k][j_1];
                    k = k + 1;
                }
                row_1 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(row_1), java.util.stream.IntStream.of(cell)).toArray()));
                j_1 = j_1 + 1;
            }
            result = ((int[][])(appendObj(result, row_1)));
            i_1 = i_1 + 1;
        }
        return result;
    }

    static int[][] matrix_pow(int[][] base, int exp) {
        int[][] result_1 = ((int[][])(identity(base.length)));
        int[][] b = ((int[][])(base));
        int e = exp;
        while (e > 0) {
            if (Math.floorMod(e, 2) == 1) {
                result_1 = ((int[][])(matrix_mul(((int[][])(result_1)), ((int[][])(b)))));
            }
            b = ((int[][])(matrix_mul(((int[][])(b)), ((int[][])(b)))));
            e = e / 2;
        }
        return result_1;
    }

    static int fibonacci_with_matrix_exponentiation(int n, int f1, int f2) {
        if (n == 1) {
            return f1;
        }
        if (n == 2) {
            return f2;
        }
        int[][] base = ((int[][])(new int[][]{new int[]{1, 1}, new int[]{1, 0}}));
        int[][] m = ((int[][])(matrix_pow(((int[][])(base)), n - 2)));
        return f2 * m[0][0] + f1 * m[0][1];
    }

    static int simple_fibonacci(int n, int f1, int f2) {
        if (n == 1) {
            return f1;
        }
        if (n == 2) {
            return f2;
        }
        int a = f1;
        int b_1 = f2;
        int count = n - 2;
        while (count > 0) {
            int tmp = a + b_1;
            a = b_1;
            b_1 = tmp;
            count = count - 1;
        }
        return b_1;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(_p(fibonacci_with_matrix_exponentiation(1, 5, 6)));
            System.out.println(_p(fibonacci_with_matrix_exponentiation(2, 10, 11)));
            System.out.println(_p(fibonacci_with_matrix_exponentiation(13, 0, 1)));
            System.out.println(_p(fibonacci_with_matrix_exponentiation(10, 5, 9)));
            System.out.println(_p(fibonacci_with_matrix_exponentiation(9, 2, 3)));
            System.out.println(_p(simple_fibonacci(1, 5, 6)));
            System.out.println(_p(simple_fibonacci(2, 10, 11)));
            System.out.println(_p(simple_fibonacci(13, 0, 1)));
            System.out.println(_p(simple_fibonacci(10, 5, 9)));
            System.out.println(_p(simple_fibonacci(9, 2, 3)));
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
