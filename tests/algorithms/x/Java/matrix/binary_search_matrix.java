public class Main {

    static int binary_search(int[] arr, int lower_bound, int upper_bound, int value) {
        int r = Math.floorDiv((lower_bound + upper_bound), 2);
        if (arr[r] == value) {
            return r;
        }
        if (lower_bound >= upper_bound) {
            return -1;
        }
        if (arr[r] < value) {
            return binary_search(((int[])(arr)), r + 1, upper_bound, value);
        }
        return binary_search(((int[])(arr)), lower_bound, r - 1, value);
    }

    static int[] mat_bin_search(int value, int[][] matrix) {
        int index = 0;
        if (matrix[index][0] == value) {
            return new int[]{index, 0};
        }
        while (index < matrix.length && matrix[index][0] < value) {
            int r_1 = binary_search(((int[])(matrix[index])), 0, matrix[index].length - 1, value);
            if (r_1 != (-1)) {
                return new int[]{index, r_1};
            }
            index = index + 1;
        }
        return new int[]{-1, -1};
    }

    static void main() {
        int[] row = ((int[])(new int[]{1, 4, 7, 11, 15}));
        System.out.println(_p(binary_search(((int[])(row)), 0, row.length - 1, 1)));
        System.out.println(_p(binary_search(((int[])(row)), 0, row.length - 1, 23)));
        int[][] matrix = ((int[][])(new int[][]{new int[]{1, 4, 7, 11, 15}, new int[]{2, 5, 8, 12, 19}, new int[]{3, 6, 9, 16, 22}, new int[]{10, 13, 14, 17, 24}, new int[]{18, 21, 23, 26, 30}}));
        System.out.println(_p(mat_bin_search(1, ((int[][])(matrix)))));
        System.out.println(_p(mat_bin_search(34, ((int[][])(matrix)))));
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
