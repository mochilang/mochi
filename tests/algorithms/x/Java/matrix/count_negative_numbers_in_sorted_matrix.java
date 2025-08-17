public class Main {
    static long[][] grid;
    static long[][][] test_grids;
    static long[] results_bin = ((long[])(new long[]{}));
    static long i_8 = 0L;
    static long[] results_brute = ((long[])(new long[]{}));
    static long[] results_break = ((long[])(new long[]{}));

    static long[][] generate_large_matrix() {
        long[][] result = ((long[][])(new long[][]{}));
        long i_1 = 0L;
        while ((long)(i_1) < 1000L) {
            long[] row_1 = ((long[])(new long[]{}));
            long j_1 = (long)(1000L - (long)(i_1));
            while ((long)(j_1) > (long)(((long)(-1000) - (long)(i_1)))) {
                row_1 = ((long[])(appendLong(row_1, (long)(j_1))));
                j_1 = (long)((long)(j_1) - 1L);
            }
            result = ((long[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(result), java.util.stream.Stream.of(new long[][]{row_1})).toArray(long[][]::new)));
            i_1 = (long)((long)(i_1) + 1L);
        }
        return result;
    }

    static long find_negative_index(long[] arr) {
        long left = 0L;
        long right_1 = (long)((long)(arr.length) - 1L);
        if ((long)(arr.length) == 0L) {
            return 0;
        }
        if ((long)(arr[(int)(0L)]) < 0L) {
            return 0;
        }
        while ((long)(left) <= (long)(right_1)) {
            long mid_1 = Math.floorDiv(((long)(left) + (long)(right_1)), 2);
            long num_1 = (long)(arr[(int)((long)(mid_1))]);
            if ((long)(num_1) < 0L) {
                if ((long)(mid_1) == 0L) {
                    return 0;
                }
                if ((long)(arr[(int)((long)((long)(mid_1) - 1L))]) >= 0L) {
                    return mid_1;
                }
                right_1 = (long)((long)(mid_1) - 1L);
            } else {
                left = (long)((long)(mid_1) + 1L);
            }
        }
        return arr.length;
    }

    static long count_negatives_binary_search(long[][] grid) {
        long total = 0L;
        long bound_1 = (long)(grid[(int)(0L)].length);
        long i_3 = 0L;
        while ((long)(i_3) < (long)(grid.length)) {
            long[] row_3 = ((long[])(grid[(int)((long)(i_3))]));
            long idx_1 = (long)(find_negative_index(((long[])(java.util.Arrays.copyOfRange(row_3, (int)(0L), (int)((long)(bound_1)))))));
            bound_1 = (long)(idx_1);
            total = (long)((long)(total) + (long)(idx_1));
            i_3 = (long)((long)(i_3) + 1L);
        }
        return (long)(((long)(grid.length) * (long)(grid[(int)(0L)].length))) - (long)(total);
    }

    static long count_negatives_brute_force(long[][] grid) {
        long count = 0L;
        long i_5 = 0L;
        while ((long)(i_5) < (long)(grid.length)) {
            long[] row_5 = ((long[])(grid[(int)((long)(i_5))]));
            long j_3 = 0L;
            while ((long)(j_3) < (long)(row_5.length)) {
                if ((long)(row_5[(int)((long)(j_3))]) < 0L) {
                    count = (long)((long)(count) + 1L);
                }
                j_3 = (long)((long)(j_3) + 1L);
            }
            i_5 = (long)((long)(i_5) + 1L);
        }
        return count;
    }

    static long count_negatives_brute_force_with_break(long[][] grid) {
        long total_1 = 0L;
        long i_7 = 0L;
        while ((long)(i_7) < (long)(grid.length)) {
            long[] row_7 = ((long[])(grid[(int)((long)(i_7))]));
            long j_5 = 0L;
            while ((long)(j_5) < (long)(row_7.length)) {
                long number_1 = (long)(row_7[(int)((long)(j_5))]);
                if ((long)(number_1) < 0L) {
                    total_1 = (long)((long)(total_1) + (long)(((long)(row_7.length) - (long)(j_5))));
                    break;
                }
                j_5 = (long)((long)(j_5) + 1L);
            }
            i_7 = (long)((long)(i_7) + 1L);
        }
        return total_1;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            grid = ((long[][])(generate_large_matrix()));
            test_grids = ((long[][][])(new long[][][]{new long[][]{new long[]{4, 3, 2, -1}, new long[]{3, 2, 1, -1}, new long[]{1, 1, -1, -2}, new long[]{-1, -1, -2, -3}}, new long[][]{new long[]{3, 2}, new long[]{1, 0}}, new long[][]{new long[]{7, 7, 6}}, new long[][]{new long[]{7, 7, 6}, new long[]{-1, -2, -3}}, grid}));
            while ((long)(i_8) < (long)(test_grids.length)) {
                results_bin = ((long[])(appendLong(results_bin, (long)(count_negatives_binary_search(((long[][])(test_grids[(int)((long)(i_8))])))))));
                i_8 = (long)((long)(i_8) + 1L);
            }
            System.out.println(_p(results_bin));
            i_8 = 0L;
            while ((long)(i_8) < (long)(test_grids.length)) {
                results_brute = ((long[])(appendLong(results_brute, (long)(count_negatives_brute_force(((long[][])(test_grids[(int)((long)(i_8))])))))));
                i_8 = (long)((long)(i_8) + 1L);
            }
            System.out.println(_p(results_brute));
            i_8 = 0L;
            while ((long)(i_8) < (long)(test_grids.length)) {
                results_break = ((long[])(appendLong(results_break, (long)(count_negatives_brute_force_with_break(((long[][])(test_grids[(int)((long)(i_8))])))))));
                i_8 = (long)((long)(i_8) + 1L);
            }
            System.out.println(_p(results_break));
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

    static long[] appendLong(long[] arr, long v) {
        long[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
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
