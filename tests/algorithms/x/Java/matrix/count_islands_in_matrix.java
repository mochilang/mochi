public class Main {
    static long[][] grid;

    static boolean is_safe(long[][] grid, boolean[][] visited, long row, long col) {
        long rows = (long)(grid.length);
        long cols_1 = (long)(((long[])_geto(grid, (int)((long)(0)))).length);
        boolean visited_cell_1 = _getb(((boolean[])_geto(visited, (int)((long)(row)))), (int)((long)(col)));
        boolean within_bounds_1 = row >= (long)(0) && row < (long)(rows) && col >= (long)(0) && col < (long)(cols_1);
        boolean not_visited_1 = (visited_cell_1 == false);
        return within_bounds_1 && not_visited_1 && _geti(((long[])_geto(grid, (int)((long)(row)))), (int)((long)(col))) == (long)(1);
    }

    static void dfs(long[][] grid, boolean[][] visited, long row, long col) {
        long[] row_nbr = ((long[])(new long[]{-1, -1, -1, 0, 0, 1, 1, 1}));
        long[] col_nbr_1 = ((long[])(new long[]{-1, 0, 1, -1, 1, -1, 0, 1}));
((boolean[])_geto(visited, (int)((long)(row))))[(int)((long)(col))] = true;
        long k_1 = 0L;
        while ((long)(k_1) < (long)(8)) {
            long new_row_1 = (long)(row + (long)(_geti(row_nbr, (int)((long)(k_1)))));
            long new_col_1 = (long)(col + (long)(_geti(col_nbr_1, (int)((long)(k_1)))));
            if (is_safe(((long[][])(grid)), ((boolean[][])(visited)), (long)(new_row_1), (long)(new_col_1))) {
                dfs(((long[][])(grid)), ((boolean[][])(visited)), (long)(new_row_1), (long)(new_col_1));
            }
            k_1 = (long)((long)(k_1) + (long)(1));
        }
    }

    static long count_islands(long[][] grid) {
        long rows_1 = (long)(grid.length);
        long cols_3 = (long)(((long[])_geto(grid, (int)((long)(0)))).length);
        boolean[][] visited_1 = ((boolean[][])(new boolean[][]{}));
        long i_1 = 0L;
        while ((long)(i_1) < (long)(rows_1)) {
            boolean[] row_list_1 = ((boolean[])(new boolean[]{}));
            long j_1 = 0L;
            while ((long)(j_1) < (long)(cols_3)) {
                row_list_1 = ((boolean[])(appendBool(row_list_1, false)));
                j_1 = (long)((long)(j_1) + (long)(1));
            }
            visited_1 = ((boolean[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(visited_1), java.util.stream.Stream.of(row_list_1)).toArray(boolean[][]::new)));
            i_1 = (long)((long)(i_1) + (long)(1));
        }
        long count_1 = 0L;
        i_1 = (long)(0);
        while ((long)(i_1) < (long)(rows_1)) {
            long j_3 = 0L;
            while ((long)(j_3) < (long)(cols_3)) {
                if (!(Boolean)_getb(((boolean[])_geto(visited_1, (int)((long)(i_1)))), (int)((long)(j_3))) && _geti(((long[])_geto(grid, (int)((long)(i_1)))), (int)((long)(j_3))) == (long)(1)) {
                    dfs(((long[][])(grid)), ((boolean[][])(visited_1)), (long)(i_1), (long)(j_3));
                    count_1 = (long)((long)(count_1) + (long)(1));
                }
                j_3 = (long)((long)(j_3) + (long)(1));
            }
            i_1 = (long)((long)(i_1) + (long)(1));
        }
        return count_1;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            grid = ((long[][])(new long[][]{new long[]{1, 1, 0, 0, 0}, new long[]{0, 1, 0, 0, 1}, new long[]{1, 0, 0, 1, 1}, new long[]{0, 0, 0, 0, 0}, new long[]{1, 0, 1, 0, 1}}));
            System.out.println(count_islands(((long[][])(grid))));
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

    static long _geti(long[] a, int i) {
        if (a == null) return 0L;
        if (i < 0) i += a.length;
        if (i < 0 || i >= a.length) return 0L;
        return a[i];
    }

    static boolean _getb(boolean[] a, int i) {
        if (a == null) return false;
        if (i < 0) i += a.length;
        if (i < 0 || i >= a.length) return false;
        return a[i];
    }

    static Object _geto(Object[] a, int i) {
        if (a == null) return null;
        if (i < 0) i += a.length;
        if (i < 0 || i >= a.length) return null;
        return a[i];
    }
}
