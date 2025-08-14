public class Main {
    static long[][] grid1;
    static long[][] grid2;

    static long[] fill_row(long[] current_row, long[] row_above) {
current_row[(int)((long)(0))] = (long)((long)(current_row[(int)((long)(0))]) + (long)(row_above[(int)((long)(0))]));
        long cell_n_1 = 1L;
        while ((long)(cell_n_1) < (long)(current_row.length)) {
            long left_1 = (long)(current_row[(int)((long)((long)(cell_n_1) - 1L))]);
            long up_1 = (long)(row_above[(int)((long)(cell_n_1))]);
            if ((long)(left_1) < (long)(up_1)) {
current_row[(int)((long)(cell_n_1))] = (long)((long)(current_row[(int)((long)(cell_n_1))]) + (long)(left_1));
            } else {
current_row[(int)((long)(cell_n_1))] = (long)((long)(current_row[(int)((long)(cell_n_1))]) + (long)(up_1));
            }
            cell_n_1 = (long)((long)(cell_n_1) + 1L);
        }
        return current_row;
    }

    static long min_path_sum(long[][] grid) {
        if ((long)(grid.length) == 0L || (long)(grid[(int)((long)(0))].length) == 0L) {
            throw new RuntimeException(String.valueOf("The grid does not contain the appropriate information"));
        }
        long cell_n_3 = 1L;
        while ((long)(cell_n_3) < (long)(grid[(int)((long)(0))].length)) {
grid[(int)((long)(0))][(int)((long)(cell_n_3))] = (long)((long)(grid[(int)((long)(0))][(int)((long)(cell_n_3))]) + (long)(grid[(int)((long)(0))][(int)((long)((long)(cell_n_3) - 1L))]));
            cell_n_3 = (long)((long)(cell_n_3) + 1L);
        }
        long[] row_above_1 = ((long[])(grid[(int)((long)(0))]));
        long row_n_1 = 1L;
        while ((long)(row_n_1) < (long)(grid.length)) {
            long[] current_row_1 = ((long[])(grid[(int)((long)(row_n_1))]));
grid[(int)((long)(row_n_1))] = ((long[])(fill_row(((long[])(current_row_1)), ((long[])(row_above_1)))));
            row_above_1 = ((long[])(grid[(int)((long)(row_n_1))]));
            row_n_1 = (long)((long)(row_n_1) + 1L);
        }
        return grid[(int)((long)((long)(grid.length) - 1L))][(int)((long)((long)(grid[(int)((long)(0))].length) - 1L))];
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            grid1 = ((long[][])(new long[][]{new long[]{1, 3, 1}, new long[]{1, 5, 1}, new long[]{4, 2, 1}}));
            System.out.println(_p(min_path_sum(((long[][])(grid1)))));
            grid2 = ((long[][])(new long[][]{new long[]{1, 0, 5, 6, 7}, new long[]{8, 9, 0, 4, 2}, new long[]{4, 4, 4, 5, 1}, new long[]{9, 6, 3, 1, 0}, new long[]{8, 4, 3, 2, 7}}));
            System.out.println(_p(min_path_sum(((long[][])(grid2)))));
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
        if (v instanceof Double || v instanceof Float) {
            double d = ((Number) v).doubleValue();
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
