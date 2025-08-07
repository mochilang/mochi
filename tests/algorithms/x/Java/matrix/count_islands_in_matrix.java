public class Main {
    static int[][] grid;

    static boolean is_safe(int[][] grid, boolean[][] visited, int row, int col) {
        int rows = grid.length;
        int cols = grid[0].length;
        boolean visited_cell = visited[row][col];
        boolean within_bounds = row >= 0 && row < rows && col >= 0 && col < cols;
        boolean not_visited = visited_cell == false;
        return within_bounds && not_visited && grid[row][col] == 1;
    }

    static void dfs(int[][] grid, boolean[][] visited, int row, int col) {
        int[] row_nbr = ((int[])(new int[]{-1, -1, -1, 0, 0, 1, 1, 1}));
        int[] col_nbr = ((int[])(new int[]{-1, 0, 1, -1, 1, -1, 0, 1}));
visited[row][col] = true;
        int k = 0;
        while (k < 8) {
            int new_row = row + row_nbr[k];
            int new_col = col + col_nbr[k];
            if (((Boolean)(is_safe(((int[][])(grid)), ((boolean[][])(visited)), new_row, new_col)))) {
                dfs(((int[][])(grid)), ((boolean[][])(visited)), new_row, new_col);
            }
            k = k + 1;
        }
    }

    static int count_islands(int[][] grid) {
        int rows_1 = grid.length;
        int cols_1 = grid[0].length;
        boolean[][] visited = ((boolean[][])(new boolean[][]{}));
        int i = 0;
        while (i < rows_1) {
            boolean[] row_list = ((boolean[])(new boolean[]{}));
            int j = 0;
            while (j < cols_1) {
                row_list = ((boolean[])(appendBool(row_list, false)));
                j = j + 1;
            }
            visited = ((boolean[][])(appendObj(visited, row_list)));
            i = i + 1;
        }
        int count = 0;
        i = 0;
        while (i < rows_1) {
            int j_1 = 0;
            while (j_1 < cols_1) {
                if (!(Boolean)visited[i][j_1] && grid[i][j_1] == 1) {
                    dfs(((int[][])(grid)), ((boolean[][])(visited)), i, j_1);
                    count = count + 1;
                }
                j_1 = j_1 + 1;
            }
            i = i + 1;
        }
        return count;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            grid = ((int[][])(new int[][]{new int[]{1, 1, 0, 0, 0}, new int[]{0, 1, 0, 0, 1}, new int[]{1, 0, 0, 1, 1}, new int[]{0, 0, 0, 0, 0}, new int[]{1, 0, 1, 0, 1}}));
            System.out.println(count_islands(((int[][])(grid))));
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
