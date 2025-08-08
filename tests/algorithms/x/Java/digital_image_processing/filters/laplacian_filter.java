public class Main {
    static int[][] image;
    static int[][] result_1;
    static int r = 0;

    static int[][] make_matrix(int rows, int cols, int value) {
        int[][] result = ((int[][])(new int[][]{}));
        int i = 0;
        while (i < rows) {
            int[] row = ((int[])(new int[]{}));
            int j = 0;
            while (j < cols) {
                row = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(row), java.util.stream.IntStream.of(value)).toArray()));
                j = j + 1;
            }
            result = ((int[][])(appendObj(result, row)));
            i = i + 1;
        }
        return result;
    }

    static int[][] my_laplacian(int[][] src, int ksize) {
        int[][] kernel = ((int[][])(new int[][]{}));
        if (ksize == 1) {
            kernel = ((int[][])(new int[][]{new int[]{0, -1, 0}, new int[]{-1, 4, -1}, new int[]{0, -1, 0}}));
        } else         if (ksize == 3) {
            kernel = ((int[][])(new int[][]{new int[]{0, 1, 0}, new int[]{1, -4, 1}, new int[]{0, 1, 0}}));
        } else         if (ksize == 5) {
            kernel = ((int[][])(new int[][]{new int[]{0, 0, -1, 0, 0}, new int[]{0, -1, -2, -1, 0}, new int[]{-1, -2, 16, -2, -1}, new int[]{0, -1, -2, -1, 0}, new int[]{0, 0, -1, 0, 0}}));
        } else         if (ksize == 7) {
            kernel = ((int[][])(new int[][]{new int[]{0, 0, 0, -1, 0, 0, 0}, new int[]{0, 0, -2, -3, -2, 0, 0}, new int[]{0, -2, -7, -10, -7, -2, 0}, new int[]{-1, -3, -10, 68, -10, -3, -1}, new int[]{0, -2, -7, -10, -7, -2, 0}, new int[]{0, 0, -2, -3, -2, 0, 0}, new int[]{0, 0, 0, -1, 0, 0, 0}}));
        } else {
            throw new RuntimeException(String.valueOf("ksize must be in (1, 3, 5, 7)"));
        }
        int rows = src.length;
        int cols = src[0].length;
        int k = kernel.length;
        int pad = Math.floorDiv(k, 2);
        int[][] output = ((int[][])(make_matrix(rows, cols, 0)));
        int i_1 = 0;
        while (i_1 < rows) {
            int j_1 = 0;
            while (j_1 < cols) {
                int sum = 0;
                int ki = 0;
                while (ki < k) {
                    int kj = 0;
                    while (kj < k) {
                        int ii = i_1 + ki - pad;
                        int jj = j_1 + kj - pad;
                        int val = 0;
                        if (ii >= 0 && ii < rows && jj >= 0 && jj < cols) {
                            val = src[ii][jj];
                        }
                        sum = sum + val * kernel[ki][kj];
                        kj = kj + 1;
                    }
                    ki = ki + 1;
                }
output[i_1][j_1] = sum;
                j_1 = j_1 + 1;
            }
            i_1 = i_1 + 1;
        }
        return output;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            image = ((int[][])(new int[][]{new int[]{0, 0, 0, 0, 0}, new int[]{0, 10, 10, 10, 0}, new int[]{0, 10, 10, 10, 0}, new int[]{0, 10, 10, 10, 0}, new int[]{0, 0, 0, 0, 0}}));
            result_1 = ((int[][])(my_laplacian(((int[][])(image)), 3)));
            r = 0;
            while (r < result_1.length) {
                String row_str = "[";
                int c = 0;
                while (c < result_1[r].length) {
                    row_str = row_str + _p(_geti(result_1[r], c));
                    if (c + 1 < result_1[r].length) {
                        row_str = row_str + ", ";
                    }
                    c = c + 1;
                }
                row_str = row_str + "]";
                System.out.println(row_str);
                r = r + 1;
            }
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

    static Integer _geti(int[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
