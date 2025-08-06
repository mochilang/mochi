public class Main {
    static int[][] grid1;
    static int[][] grid2;

    static int[] fill_row(int[] current_row, int[] row_above) {
current_row[0] = current_row[0] + row_above[0];
        int cell_n = 1;
        while (cell_n < current_row.length) {
            int left = current_row[cell_n - 1];
            int up = row_above[cell_n];
            if (left < up) {
current_row[cell_n] = current_row[cell_n] + left;
            } else {
current_row[cell_n] = current_row[cell_n] + up;
            }
            cell_n = cell_n + 1;
        }
        return current_row;
    }

    static int min_path_sum(int[][] grid) {
        if (grid.length == 0 || grid[0].length == 0) {
            throw new RuntimeException(String.valueOf("The grid does not contain the appropriate information"));
        }
        int cell_n_1 = 1;
        while (cell_n_1 < grid[0].length) {
grid[0][cell_n_1] = grid[0][cell_n_1] + grid[0][cell_n_1 - 1];
            cell_n_1 = cell_n_1 + 1;
        }
        int[] row_above = ((int[])(grid[0]));
        int row_n = 1;
        while (row_n < grid.length) {
            int[] current_row = ((int[])(grid[row_n]));
grid[row_n] = ((int[])(fill_row(((int[])(current_row)), ((int[])(row_above)))));
            row_above = ((int[])(grid[row_n]));
            row_n = row_n + 1;
        }
        return grid[grid.length - 1][grid[0].length - 1];
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            grid1 = ((int[][])(new int[][]{new int[]{1, 3, 1}, new int[]{1, 5, 1}, new int[]{4, 2, 1}}));
            System.out.println(_p(min_path_sum(((int[][])(grid1)))));
            grid2 = ((int[][])(new int[][]{new int[]{1, 0, 5, 6, 7}, new int[]{8, 9, 0, 4, 2}, new int[]{4, 4, 4, 5, 1}, new int[]{9, 6, 3, 1, 0}, new int[]{8, 4, 3, 2, 7}}));
            System.out.println(_p(min_path_sum(((int[][])(grid2)))));
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
