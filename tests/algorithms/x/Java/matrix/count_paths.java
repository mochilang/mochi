public class Main {

    static long depth_first_search(long[][] grid, long row, long col, boolean[][] visit) {
        long row_length = (long)(grid.length);
        long col_length_1 = (long)(grid[(int)(0L)].length);
        if ((long)(row) < 0L || (long)(col) < 0L || (long)(row) == (long)(row_length) || (long)(col) == (long)(col_length_1)) {
            return 0;
        }
        if (visit[(int)((long)(row))][(int)((long)(col))]) {
            return 0;
        }
        if ((long)(grid[(int)((long)(row))][(int)((long)(col))]) == 1L) {
            return 0;
        }
        if ((long)(row) == (long)((long)(row_length) - 1L) && (long)(col) == (long)((long)(col_length_1) - 1L)) {
            return 1;
        }
visit[(int)((long)(row))][(int)((long)(col))] = true;
        long count_1 = 0L;
        count_1 = (long)((long)(count_1) + (long)(depth_first_search(((long[][])(grid)), (long)((long)(row) + 1L), (long)(col), ((boolean[][])(visit)))));
        count_1 = (long)((long)(count_1) + (long)(depth_first_search(((long[][])(grid)), (long)((long)(row) - 1L), (long)(col), ((boolean[][])(visit)))));
        count_1 = (long)((long)(count_1) + (long)(depth_first_search(((long[][])(grid)), (long)(row), (long)((long)(col) + 1L), ((boolean[][])(visit)))));
        count_1 = (long)((long)(count_1) + (long)(depth_first_search(((long[][])(grid)), (long)(row), (long)((long)(col) - 1L), ((boolean[][])(visit)))));
visit[(int)((long)(row))][(int)((long)(col))] = false;
        return count_1;
    }

    static long count_paths(long[][] grid) {
        long rows = (long)(grid.length);
        long cols_1 = (long)(grid[(int)(0L)].length);
        boolean[][] visit_1 = ((boolean[][])(new boolean[][]{}));
        long i_1 = 0L;
        while ((long)(i_1) < (long)(rows)) {
            boolean[] row_visit_1 = ((boolean[])(new boolean[]{}));
            long j_1 = 0L;
            while ((long)(j_1) < (long)(cols_1)) {
                row_visit_1 = ((boolean[])(appendBool(row_visit_1, false)));
                j_1 = (long)((long)(j_1) + 1L);
            }
            visit_1 = ((boolean[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(visit_1), java.util.stream.Stream.of(new boolean[][]{row_visit_1})).toArray(boolean[][]::new)));
            i_1 = (long)((long)(i_1) + 1L);
        }
        return depth_first_search(((long[][])(grid)), 0L, 0L, ((boolean[][])(visit_1)));
    }

    static void main() {
        long[][] grid1 = ((long[][])(new long[][]{new long[]{0, 0, 0, 0}, new long[]{1, 1, 0, 0}, new long[]{0, 0, 0, 1}, new long[]{0, 1, 0, 0}}));
        System.out.println(_p(count_paths(((long[][])(grid1)))));
        long[][] grid2_1 = ((long[][])(new long[][]{new long[]{0, 0, 0, 0, 0}, new long[]{0, 1, 1, 1, 0}, new long[]{0, 1, 1, 1, 0}, new long[]{0, 0, 0, 0, 0}}));
        System.out.println(_p(count_paths(((long[][])(grid2_1)))));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            main();
            long _benchDuration = _now() - _benchStart;
            long _benchMemory = _mem() - _benchMem;
            System.out.println("{\"duration_us\": " + _benchDuration + ", \"memory_bytes\": " + _benchMemory + ", \"name\": \"main\"}");
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
