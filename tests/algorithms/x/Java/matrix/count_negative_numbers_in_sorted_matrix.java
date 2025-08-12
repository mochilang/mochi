public class Main {
    static long[][] grid;
    static long[][][] test_grids;
    static long[] results_bin = new long[0];
    static long i_8 = 0;
    static long[] results_brute = new long[0];
    static long[] results_break = new long[0];

    static long[][] generate_large_matrix() {
        long[][] result = ((long[][])(new long[][]{}));
        long i_1 = 0L;
        while ((long)(i_1) < (long)(1000)) {
            long[] row_1 = ((long[])(new long[]{}));
            long j_1 = (long)((long)(1000) - (long)(i_1));
            while ((long)(j_1) > (long)(((long)(-1000) - (long)(i_1)))) {
                row_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(row_1), java.util.stream.LongStream.of((long)(j_1))).toArray()));
                j_1 = (long)((long)(j_1) - (long)(1));
            }
            result = ((long[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(result), java.util.stream.Stream.of(row_1)).toArray(long[][]::new)));
            i_1 = (long)((long)(i_1) + (long)(1));
        }
        return result;
    }

    static long find_negative_index(long[] arr) {
        long left = 0L;
        long right_1 = (long)((long)(arr.length) - (long)(1));
        if ((long)(arr.length) == (long)(0)) {
            return 0;
        }
        if (_geti(arr, (int)((long)(0))) < (long)(0)) {
            return 0;
        }
        while ((long)(left) <= (long)(right_1)) {
            long mid_1 = Math.floorDiv(((long)(left) + (long)(right_1)), 2);
            long num_1 = _geti(arr, (int)((long)(mid_1)));
            if (num_1 < (long)(0)) {
                if ((long)(mid_1) == (long)(0)) {
                    return 0;
                }
                if (_geti(arr, (int)((long)((long)(mid_1) - (long)(1)))) >= (long)(0)) {
                    return mid_1;
                }
                right_1 = (long)((long)(mid_1) - (long)(1));
            } else {
                left = (long)((long)(mid_1) + (long)(1));
            }
        }
        return arr.length;
    }

    static long count_negatives_binary_search(long[][] grid) {
        long total = 0L;
        long bound_1 = (long)(((long[])_geto(grid, (int)((long)(0)))).length);
        long i_3 = 0L;
        while ((long)(i_3) < (long)(grid.length)) {
            long[] row_3 = ((long[])(((long[])_geto(grid, (int)((long)(i_3))))));
            long idx_1 = find_negative_index(((long[])(java.util.Arrays.copyOfRange(row_3, (int)((long)(0)), (int)((long)(bound_1))))));
            bound_1 = idx_1;
            total = (long)((long)(total) + idx_1);
            i_3 = (long)((long)(i_3) + (long)(1));
        }
        return (long)(((long)(grid.length) * (long)(((long[])_geto(grid, (int)((long)(0)))).length))) - (long)(total);
    }

    static long count_negatives_brute_force(long[][] grid) {
        long count = 0L;
        long i_5 = 0L;
        while ((long)(i_5) < (long)(grid.length)) {
            long[] row_5 = ((long[])(((long[])_geto(grid, (int)((long)(i_5))))));
            long j_3 = 0L;
            while ((long)(j_3) < (long)(row_5.length)) {
                if (_geti(row_5, (int)((long)(j_3))) < (long)(0)) {
                    count = (long)((long)(count) + (long)(1));
                }
                j_3 = (long)((long)(j_3) + (long)(1));
            }
            i_5 = (long)((long)(i_5) + (long)(1));
        }
        return count;
    }

    static long count_negatives_brute_force_with_break(long[][] grid) {
        long total_1 = 0L;
        long i_7 = 0L;
        while ((long)(i_7) < (long)(grid.length)) {
            long[] row_7 = ((long[])(((long[])_geto(grid, (int)((long)(i_7))))));
            long j_5 = 0L;
            while ((long)(j_5) < (long)(row_7.length)) {
                long number_1 = _geti(row_7, (int)((long)(j_5)));
                if (number_1 < (long)(0)) {
                    total_1 = (long)((long)(total_1) + (long)(((long)(row_7.length) - (long)(j_5))));
                    break;
                }
                j_5 = (long)((long)(j_5) + (long)(1));
            }
            i_7 = (long)((long)(i_7) + (long)(1));
        }
        return total_1;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            grid = ((long[][])(generate_large_matrix()));
            test_grids = ((long[][][])(new Object[]{new long[][]{new long[]{4, 3, 2, -1}, new long[]{3, 2, 1, -1}, new long[]{1, 1, -1, -2}, new long[]{-1, -1, -2, -3}}, new long[][]{new long[]{3, 2}, new long[]{1, 0}}, new long[][]{new long[]{7, 7, 6}}, new long[][]{new long[]{7, 7, 6}, new long[]{-1, -2, -3}}, grid}));
            results_bin = ((long[])(new long[]{}));
            i_8 = (long)(0);
            while ((long)(i_8) < (long)(test_grids.length)) {
                results_bin = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(results_bin), java.util.stream.LongStream.of(count_negatives_binary_search(((long[][])(((long[][])_geto(test_grids, (int)((long)(i_8))))))))).toArray()));
                i_8 = (long)((long)(i_8) + (long)(1));
            }
            System.out.println(_p(results_bin));
            results_brute = ((long[])(new long[]{}));
            i_8 = (long)(0);
            while ((long)(i_8) < (long)(test_grids.length)) {
                results_brute = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(results_brute), java.util.stream.LongStream.of(count_negatives_brute_force(((long[][])(((long[][])_geto(test_grids, (int)((long)(i_8))))))))).toArray()));
                i_8 = (long)((long)(i_8) + (long)(1));
            }
            System.out.println(_p(results_brute));
            results_break = ((long[])(new long[]{}));
            i_8 = (long)(0);
            while ((long)(i_8) < (long)(test_grids.length)) {
                results_break = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(results_break), java.util.stream.LongStream.of(count_negatives_brute_force_with_break(((long[][])(((long[][])_geto(test_grids, (int)((long)(i_8))))))))).toArray()));
                i_8 = (long)((long)(i_8) + (long)(1));
            }
            System.out.println(_p(results_break));
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
            if (d == Math.rint(d)) return String.valueOf((long) d);
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }

    static long _geti(long[] a, int i) {
        if (a == null) return 0L;
        if (i < 0) i += a.length;
        if (i < 0 || i >= a.length) return 0L;
        return a[i];
    }

    static Object _geto(Object[] a, int i) {
        if (a == null) return null;
        if (i < 0) i += a.length;
        if (i < 0 || i >= a.length) return null;
        return a[i];
    }
}
