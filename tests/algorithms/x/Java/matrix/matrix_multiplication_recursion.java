public class Main {
    static int[][] matrix_1_to_4;
    static int[][] matrix_5_to_8;
    static int[][] matrix_count_up;
    static int[][] matrix_unordered;

    static boolean is_square(int[][] matrix) {
        int n = matrix.length;
        int i = 0;
        while (i < n) {
            if (matrix[i].length != n) {
                return false;
            }
            i = i + 1;
        }
        return true;
    }

    static int[][] matrix_multiply(int[][] a, int[][] b) {
        int rows = a.length;
        int cols = b[0].length;
        int inner = b.length;
        int[][] result = ((int[][])(new int[][]{}));
        int i_1 = 0;
        while (i_1 < rows) {
            int[] row = ((int[])(new int[]{}));
            int j = 0;
            while (j < cols) {
                int sum = 0;
                int k = 0;
                while (k < inner) {
                    sum = sum + a[i_1][k] * b[k][j];
                    k = k + 1;
                }
                row = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(row), java.util.stream.IntStream.of(sum)).toArray()));
                j = j + 1;
            }
            result = ((int[][])(appendObj(result, row)));
            i_1 = i_1 + 1;
        }
        return result;
    }

    static void multiply(int i, int j, int k, int[][] a, int[][] b, int[][] result, int n, int m) {
        if (i >= n) {
            return;
        }
        if (j >= m) {
            multiply(i + 1, 0, 0, ((int[][])(a)), ((int[][])(b)), ((int[][])(result)), n, m);
            return;
        }
        if (k >= b.length) {
            multiply(i, j + 1, 0, ((int[][])(a)), ((int[][])(b)), ((int[][])(result)), n, m);
            return;
        }
result[i][j] = result[i][j] + a[i][k] * b[k][j];
        multiply(i, j, k + 1, ((int[][])(a)), ((int[][])(b)), ((int[][])(result)), n, m);
    }

    static int[][] matrix_multiply_recursive(int[][] a, int[][] b) {
        if (a.length == 0 || b.length == 0) {
            return new int[][]{};
        }
        if (a.length != b.length || (!(Boolean)is_square(((int[][])(a)))) || (!(Boolean)is_square(((int[][])(b))))) {
            throw new RuntimeException(String.valueOf("Invalid matrix dimensions"));
        }
        int n_1 = a.length;
        int m = b[0].length;
        int[][] result_1 = ((int[][])(new int[][]{}));
        int i_2 = 0;
        while (i_2 < n_1) {
            int[] row_1 = ((int[])(new int[]{}));
            int j_1 = 0;
            while (j_1 < m) {
                row_1 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(row_1), java.util.stream.IntStream.of(0)).toArray()));
                j_1 = j_1 + 1;
            }
            result_1 = ((int[][])(appendObj(result_1, row_1)));
            i_2 = i_2 + 1;
        }
        multiply(0, 0, 0, ((int[][])(a)), ((int[][])(b)), ((int[][])(result_1)), n_1, m);
        return result_1;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            matrix_1_to_4 = ((int[][])(new int[][]{new int[]{1, 2}, new int[]{3, 4}}));
            matrix_5_to_8 = ((int[][])(new int[][]{new int[]{5, 6}, new int[]{7, 8}}));
            matrix_count_up = ((int[][])(new int[][]{new int[]{1, 2, 3, 4}, new int[]{5, 6, 7, 8}, new int[]{9, 10, 11, 12}, new int[]{13, 14, 15, 16}}));
            matrix_unordered = ((int[][])(new int[][]{new int[]{5, 8, 1, 2}, new int[]{6, 7, 3, 0}, new int[]{4, 5, 9, 1}, new int[]{2, 6, 10, 14}}));
            System.out.println(matrix_multiply_recursive(((int[][])(matrix_1_to_4)), ((int[][])(matrix_5_to_8))));
            System.out.println(matrix_multiply_recursive(((int[][])(matrix_count_up)), ((int[][])(matrix_unordered))));
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
}
