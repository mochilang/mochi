public class Main {

    static int depth_first_search(int[][] grid, int row, int col, boolean[][] visit) {
        int row_length = grid.length;
        int col_length = grid[0].length;
        if (row < 0 || col < 0 || row == row_length || col == col_length) {
            return 0;
        }
        if (((Boolean)(visit[row][col]))) {
            return 0;
        }
        if (grid[row][col] == 1) {
            return 0;
        }
        if (row == row_length - 1 && col == col_length - 1) {
            return 1;
        }
visit[row][col] = true;
        int count = 0;
        count = count + depth_first_search(((int[][])(grid)), row + 1, col, ((boolean[][])(visit)));
        count = count + depth_first_search(((int[][])(grid)), row - 1, col, ((boolean[][])(visit)));
        count = count + depth_first_search(((int[][])(grid)), row, col + 1, ((boolean[][])(visit)));
        count = count + depth_first_search(((int[][])(grid)), row, col - 1, ((boolean[][])(visit)));
visit[row][col] = false;
        return count;
    }

    static int count_paths(int[][] grid) {
        int rows = grid.length;
        int cols = grid[0].length;
        boolean[][] visit = ((boolean[][])(new boolean[][]{}));
        int i = 0;
        while (i < rows) {
            boolean[] row_visit = ((boolean[])(new boolean[]{}));
            int j = 0;
            while (j < cols) {
                row_visit = ((boolean[])(appendBool(row_visit, false)));
                j = j + 1;
            }
            visit = ((boolean[][])(appendObj(visit, row_visit)));
            i = i + 1;
        }
        return depth_first_search(((int[][])(grid)), 0, 0, ((boolean[][])(visit)));
    }

    static void main() {
        int[][] grid1 = ((int[][])(new int[][]{new int[]{0, 0, 0, 0}, new int[]{1, 1, 0, 0}, new int[]{0, 0, 0, 1}, new int[]{0, 1, 0, 0}}));
        System.out.println(_p(count_paths(((int[][])(grid1)))));
        int[][] grid2 = ((int[][])(new int[][]{new int[]{0, 0, 0, 0, 0}, new int[]{0, 1, 1, 1, 0}, new int[]{0, 1, 1, 1, 0}, new int[]{0, 0, 0, 0, 0}}));
        System.out.println(_p(count_paths(((int[][])(grid2)))));
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
