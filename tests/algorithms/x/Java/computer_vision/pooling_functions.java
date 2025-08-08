public class Main {

    static int[][] maxpooling(int[][] arr, int size, int stride) {
        int n = arr.length;
        if (n == 0 || arr[0].length != n) {
            throw new RuntimeException(String.valueOf("The input array is not a square matrix"));
        }
        int[][] result = ((int[][])(new int[][]{}));
        int i = 0;
        while (i + size <= n) {
            int[] row = ((int[])(new int[]{}));
            int j = 0;
            while (j + size <= n) {
                int max_val = arr[i][j];
                int r = i;
                while (r < i + size) {
                    int c = j;
                    while (c < j + size) {
                        int val = arr[r][c];
                        if (val > max_val) {
                            max_val = val;
                        }
                        c = c + 1;
                    }
                    r = r + 1;
                }
                row = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(row), java.util.stream.IntStream.of(max_val)).toArray()));
                j = j + stride;
            }
            result = ((int[][])(appendObj((int[][])result, row)));
            i = i + stride;
        }
        return result;
    }

    static int[][] avgpooling(int[][] arr, int size, int stride) {
        int n_1 = arr.length;
        if (n_1 == 0 || arr[0].length != n_1) {
            throw new RuntimeException(String.valueOf("The input array is not a square matrix"));
        }
        int[][] result_1 = ((int[][])(new int[][]{}));
        int i_1 = 0;
        while (i_1 + size <= n_1) {
            int[] row_1 = ((int[])(new int[]{}));
            int j_1 = 0;
            while (j_1 + size <= n_1) {
                int sum = 0;
                int r_1 = i_1;
                while (r_1 < i_1 + size) {
                    int c_1 = j_1;
                    while (c_1 < j_1 + size) {
                        sum = sum + arr[r_1][c_1];
                        c_1 = c_1 + 1;
                    }
                    r_1 = r_1 + 1;
                }
                row_1 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(row_1), java.util.stream.IntStream.of(Math.floorDiv(sum, (size * size)))).toArray()));
                j_1 = j_1 + stride;
            }
            result_1 = ((int[][])(appendObj((int[][])result_1, row_1)));
            i_1 = i_1 + stride;
        }
        return result_1;
    }

    static void print_matrix(int[][] mat) {
        int i_2 = 0;
        while (i_2 < mat.length) {
            String line = "";
            int j_2 = 0;
            while (j_2 < mat[i_2].length) {
                line = line + _p(_geti(mat[i_2], j_2));
                if (j_2 < mat[i_2].length - 1) {
                    line = line + " ";
                }
                j_2 = j_2 + 1;
            }
            System.out.println(line);
            i_2 = i_2 + 1;
        }
    }

    static void main() {
        int[][] arr1 = ((int[][])(new int[][]{new int[]{1, 2, 3, 4}, new int[]{5, 6, 7, 8}, new int[]{9, 10, 11, 12}, new int[]{13, 14, 15, 16}}));
        int[][] arr2 = ((int[][])(new int[][]{new int[]{147, 180, 122}, new int[]{241, 76, 32}, new int[]{126, 13, 157}}));
        print_matrix(((int[][])(maxpooling(((int[][])(arr1)), 2, 2))));
        print_matrix(((int[][])(maxpooling(((int[][])(arr2)), 2, 1))));
        print_matrix(((int[][])(avgpooling(((int[][])(arr1)), 2, 2))));
        print_matrix(((int[][])(avgpooling(((int[][])(arr2)), 2, 1))));
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

    static Integer _geti(int[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
