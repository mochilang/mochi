public class Main {
    static int[][] matrix1;
    static int[][] matrix2;

    static int[] bubble_sort(int[] a) {
        int[] arr = ((int[])(a));
        int n = arr.length;
        int i = 0;
        while (i < n) {
            int j = 0;
            while (j + 1 < n - i) {
                if (arr[j] > arr[j + 1]) {
                    int temp = arr[j];
arr[j] = arr[j + 1];
arr[j + 1] = temp;
                }
                j = j + 1;
            }
            i = i + 1;
        }
        return arr;
    }

    static int median(int[][] matrix) {
        int[] linear = ((int[])(new int[]{}));
        int i_1 = 0;
        while (i_1 < matrix.length) {
            int[] row = ((int[])(matrix[i_1]));
            int j_1 = 0;
            while (j_1 < row.length) {
                linear = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(linear), java.util.stream.IntStream.of(row[j_1])).toArray()));
                j_1 = j_1 + 1;
            }
            i_1 = i_1 + 1;
        }
        int[] sorted = ((int[])(bubble_sort(((int[])(linear)))));
        int mid = Math.floorDiv((sorted.length - 1), 2);
        return sorted[mid];
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            matrix1 = ((int[][])(new int[][]{new int[]{1, 3, 5}, new int[]{2, 6, 9}, new int[]{3, 6, 9}}));
            System.out.println(_p(median(((int[][])(matrix1)))));
            matrix2 = ((int[][])(new int[][]{new int[]{1, 2, 3}, new int[]{4, 5, 6}}));
            System.out.println(_p(median(((int[][])(matrix2)))));
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
