public class Main {

    static long index_of(long[] xs, long x) {
        long i = 0L;
        while ((long)(i) < (long)(xs.length)) {
            if ((long)(xs[(int)((long)(i))]) == (long)(x)) {
                return i;
            }
            i = (long)((long)(i) + 1L);
        }
        return -1;
    }

    static long[] majority_vote(long[] votes, long votes_needed_to_win) {
        if ((long)(votes_needed_to_win) < 2L) {
            return new long[]{};
        }
        long[] candidates_1 = ((long[])(new long[]{}));
        long[] counts_1 = ((long[])(new long[]{}));
        long i_2 = 0L;
        while ((long)(i_2) < (long)(votes.length)) {
            long v_1 = (long)(votes[(int)((long)(i_2))]);
            long idx_1 = (long)(index_of(((long[])(candidates_1)), (long)(v_1)));
            if ((long)(idx_1) != (long)(-1)) {
counts_1[(int)((long)(idx_1))] = (long)((long)(counts_1[(int)((long)(idx_1))]) + 1L);
            } else             if ((long)(candidates_1.length) < (long)((long)(votes_needed_to_win) - 1L)) {
                candidates_1 = ((long[])(appendLong(candidates_1, (long)(v_1))));
                counts_1 = ((long[])(appendLong(counts_1, 1L)));
            } else {
                long j_1 = 0L;
                while ((long)(j_1) < (long)(counts_1.length)) {
counts_1[(int)((long)(j_1))] = (long)((long)(counts_1[(int)((long)(j_1))]) - 1L);
                    j_1 = (long)((long)(j_1) + 1L);
                }
                long[] new_candidates_1 = ((long[])(new long[]{}));
                long[] new_counts_1 = ((long[])(new long[]{}));
                j_1 = 0L;
                while ((long)(j_1) < (long)(candidates_1.length)) {
                    if ((long)(counts_1[(int)((long)(j_1))]) > 0L) {
                        new_candidates_1 = ((long[])(appendLong(new_candidates_1, (long)(candidates_1[(int)((long)(j_1))]))));
                        new_counts_1 = ((long[])(appendLong(new_counts_1, (long)(counts_1[(int)((long)(j_1))]))));
                    }
                    j_1 = (long)((long)(j_1) + 1L);
                }
                candidates_1 = ((long[])(new_candidates_1));
                counts_1 = ((long[])(new_counts_1));
            }
            i_2 = (long)((long)(i_2) + 1L);
        }
        long[] final_counts_1 = ((long[])(new long[]{}));
        long j_3 = 0L;
        while ((long)(j_3) < (long)(candidates_1.length)) {
            final_counts_1 = ((long[])(appendLong(final_counts_1, 0L)));
            j_3 = (long)((long)(j_3) + 1L);
        }
        i_2 = 0L;
        while ((long)(i_2) < (long)(votes.length)) {
            long v_3 = (long)(votes[(int)((long)(i_2))]);
            long idx_3 = (long)(index_of(((long[])(candidates_1)), (long)(v_3)));
            if ((long)(idx_3) != (long)(-1)) {
final_counts_1[(int)((long)(idx_3))] = (long)((long)(final_counts_1[(int)((long)(idx_3))]) + 1L);
            }
            i_2 = (long)((long)(i_2) + 1L);
        }
        long[] result_1 = ((long[])(new long[]{}));
        j_3 = 0L;
        while ((long)(j_3) < (long)(candidates_1.length)) {
            if ((long)((long)(final_counts_1[(int)((long)(j_3))]) * (long)(votes_needed_to_win)) > (long)(votes.length)) {
                result_1 = ((long[])(appendLong(result_1, (long)(candidates_1[(int)((long)(j_3))]))));
            }
            j_3 = (long)((long)(j_3) + 1L);
        }
        return result_1;
    }

    static void main() {
        long[] votes = ((long[])(new long[]{1, 2, 2, 3, 1, 3, 2}));
        System.out.println(_p(majority_vote(((long[])(votes)), 3L)));
        System.out.println(_p(majority_vote(((long[])(votes)), 2L)));
        System.out.println(_p(majority_vote(((long[])(votes)), 4L)));
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
