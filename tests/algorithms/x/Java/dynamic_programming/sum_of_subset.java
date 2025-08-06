public class Main {

    static boolean[][] create_bool_matrix(int rows, int cols) {
        boolean[][] matrix = ((boolean[][])(new boolean[][]{}));
        int i = 0;
        while (i <= rows) {
            boolean[] row = ((boolean[])(new boolean[]{}));
            int j = 0;
            while (j <= cols) {
                row = ((boolean[])(appendBool(row, false)));
                j = j + 1;
            }
            matrix = ((boolean[][])(appendObj(matrix, row)));
            i = i + 1;
        }
        return matrix;
    }

    static boolean is_sum_subset(int[] arr, int required_sum) {
        int arr_len = arr.length;
        boolean[][] subset = ((boolean[][])(create_bool_matrix(arr_len, required_sum)));
        int i_1 = 0;
        while (i_1 <= arr_len) {
subset[i_1][0] = true;
            i_1 = i_1 + 1;
        }
        int j_1 = 1;
        while (j_1 <= required_sum) {
subset[0][j_1] = false;
            j_1 = j_1 + 1;
        }
        i_1 = 1;
        while (i_1 <= arr_len) {
            j_1 = 1;
            while (j_1 <= required_sum) {
                if (arr[i_1 - 1] > j_1) {
subset[i_1][j_1] = subset[i_1 - 1][j_1];
                }
                if (arr[i_1 - 1] <= j_1) {
subset[i_1][j_1] = ((Boolean)(subset[i_1 - 1][j_1])) || ((Boolean)(subset[i_1 - 1][j_1 - arr[i_1 - 1]]));
                }
                j_1 = j_1 + 1;
            }
            i_1 = i_1 + 1;
        }
        return subset[arr_len][required_sum];
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(is_sum_subset(((int[])(new int[]{2, 4, 6, 8})), 5));
            System.out.println(is_sum_subset(((int[])(new int[]{2, 4, 6, 8})), 14));
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

    static boolean[] appendBool(boolean[] arr, boolean v) {
        boolean[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }
}
