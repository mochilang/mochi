public class Main {
    static long[][] donor_pref;
    static long[][] recipient_pref;

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

    static long[] remove_item(long[] xs, long x) {
        long[] res = ((long[])(new long[]{}));
        boolean removed_1 = false;
        long i_2 = 0L;
        while ((long)(i_2) < (long)(xs.length)) {
            if (!removed_1 && (long)(xs[(int)((long)(i_2))]) == (long)(x)) {
                removed_1 = true;
            } else {
                res = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(res), java.util.stream.LongStream.of((long)(xs[(int)((long)(i_2))]))).toArray()));
            }
            i_2 = (long)((long)(i_2) + 1L);
        }
        return res;
    }

    static long[] stable_matching(long[][] donor_pref, long[][] recipient_pref) {
        if ((long)(donor_pref.length) != (long)(recipient_pref.length)) {
            throw new RuntimeException(String.valueOf("unequal groups"));
        }
        long n_1 = (long)(donor_pref.length);
        long[] unmatched_1 = ((long[])(new long[]{}));
        long i_4 = 0L;
        while ((long)(i_4) < (long)(n_1)) {
            unmatched_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(unmatched_1), java.util.stream.LongStream.of((long)(i_4))).toArray()));
            i_4 = (long)((long)(i_4) + 1L);
        }
        long[] donor_record_1 = ((long[])(new long[]{}));
        i_4 = 0L;
        while ((long)(i_4) < (long)(n_1)) {
            donor_record_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(donor_record_1), java.util.stream.LongStream.of((long)(-1))).toArray()));
            i_4 = (long)((long)(i_4) + 1L);
        }
        long[] rec_record_1 = ((long[])(new long[]{}));
        i_4 = 0L;
        while ((long)(i_4) < (long)(n_1)) {
            rec_record_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(rec_record_1), java.util.stream.LongStream.of((long)(-1))).toArray()));
            i_4 = (long)((long)(i_4) + 1L);
        }
        long[] num_donations_1 = ((long[])(new long[]{}));
        i_4 = 0L;
        while ((long)(i_4) < (long)(n_1)) {
            num_donations_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(num_donations_1), java.util.stream.LongStream.of(0L)).toArray()));
            i_4 = (long)((long)(i_4) + 1L);
        }
        while ((long)(unmatched_1.length) > 0L) {
            long donor_1 = (long)(unmatched_1[(int)((long)(0))]);
            long[] donor_preference_1 = ((long[])(donor_pref[(int)((long)(donor_1))]));
            long recipient_1 = (long)(donor_preference_1[(int)((long)(num_donations_1[(int)((long)(donor_1))]))]);
num_donations_1[(int)((long)(donor_1))] = (long)((long)(num_donations_1[(int)((long)(donor_1))]) + 1L);
            long[] rec_preference_1 = ((long[])(recipient_pref[(int)((long)(recipient_1))]));
            long prev_donor_1 = (long)(rec_record_1[(int)((long)(recipient_1))]);
            if ((long)(prev_donor_1) != (long)(0L - 1L)) {
                long prev_index_1 = (long)(index_of(((long[])(rec_preference_1)), (long)(prev_donor_1)));
                long new_index_1 = (long)(index_of(((long[])(rec_preference_1)), (long)(donor_1)));
                if ((long)(prev_index_1) > (long)(new_index_1)) {
rec_record_1[(int)((long)(recipient_1))] = (long)(donor_1);
donor_record_1[(int)((long)(donor_1))] = (long)(recipient_1);
                    unmatched_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(unmatched_1), java.util.stream.LongStream.of((long)(prev_donor_1))).toArray()));
                    unmatched_1 = ((long[])(remove_item(((long[])(unmatched_1)), (long)(donor_1))));
                }
            } else {
rec_record_1[(int)((long)(recipient_1))] = (long)(donor_1);
donor_record_1[(int)((long)(donor_1))] = (long)(recipient_1);
                unmatched_1 = ((long[])(remove_item(((long[])(unmatched_1)), (long)(donor_1))));
            }
        }
        return donor_record_1;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            donor_pref = ((long[][])(new long[][]{new long[]{0, 1, 3, 2}, new long[]{0, 2, 3, 1}, new long[]{1, 0, 2, 3}, new long[]{0, 3, 1, 2}}));
            recipient_pref = ((long[][])(new long[][]{new long[]{3, 1, 2, 0}, new long[]{3, 1, 0, 2}, new long[]{0, 3, 1, 2}, new long[]{1, 0, 3, 2}}));
            System.out.println(_p(stable_matching(((long[][])(donor_pref)), ((long[][])(recipient_pref)))));
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
