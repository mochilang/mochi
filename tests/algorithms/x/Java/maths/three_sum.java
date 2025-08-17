public class Main {

    static long[] bubble_sort(long[] nums) {
        long[] arr = ((long[])(nums));
        long n_1 = (long)(arr.length);
        long i_1 = 0L;
        while ((long)(i_1) < (long)(n_1)) {
            long j_1 = 0L;
            while ((long)(j_1) < (long)((long)(n_1) - 1L)) {
                if ((long)(arr[(int)((long)(j_1))]) > (long)(arr[(int)((long)((long)(j_1) + 1L))])) {
                    long temp_1 = (long)(arr[(int)((long)(j_1))]);
arr[(int)((long)(j_1))] = (long)(arr[(int)((long)((long)(j_1) + 1L))]);
arr[(int)((long)((long)(j_1) + 1L))] = (long)(temp_1);
                }
                j_1 = (long)((long)(j_1) + 1L);
            }
            i_1 = (long)((long)(i_1) + 1L);
        }
        return arr;
    }

    static long[][] three_sum(long[] nums) {
        long[] sorted = ((long[])(bubble_sort(((long[])(nums)))));
        long[][] res_1 = ((long[][])(new long[][]{}));
        long n_3 = (long)(sorted.length);
        long i_3 = 0L;
        while ((long)(i_3) < (long)((long)(n_3) - 2L)) {
            if ((long)(i_3) == 0L || (long)(sorted[(int)((long)(i_3))]) != (long)(sorted[(int)((long)((long)(i_3) - 1L))])) {
                long low_1 = (long)((long)(i_3) + 1L);
                long high_1 = (long)((long)(n_3) - 1L);
                long c_1 = (long)(0L - (long)(sorted[(int)((long)(i_3))]));
                while ((long)(low_1) < (long)(high_1)) {
                    long s_1 = (long)((long)(sorted[(int)((long)(low_1))]) + (long)(sorted[(int)((long)(high_1))]));
                    if ((long)(s_1) == (long)(c_1)) {
                        long[] triple_1 = ((long[])(new long[]{sorted[(int)((long)(i_3))], sorted[(int)((long)(low_1))], sorted[(int)((long)(high_1))]}));
                        res_1 = ((long[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(res_1), java.util.stream.Stream.of(new long[][]{triple_1})).toArray(long[][]::new)));
                        while ((long)(low_1) < (long)(high_1) && (long)(sorted[(int)((long)(low_1))]) == (long)(sorted[(int)((long)((long)(low_1) + 1L))])) {
                            low_1 = (long)((long)(low_1) + 1L);
                        }
                        while ((long)(low_1) < (long)(high_1) && (long)(sorted[(int)((long)(high_1))]) == (long)(sorted[(int)((long)((long)(high_1) - 1L))])) {
                            high_1 = (long)((long)(high_1) - 1L);
                        }
                        low_1 = (long)((long)(low_1) + 1L);
                        high_1 = (long)((long)(high_1) - 1L);
                    } else                     if ((long)(s_1) < (long)(c_1)) {
                        low_1 = (long)((long)(low_1) + 1L);
                    } else {
                        high_1 = (long)((long)(high_1) - 1L);
                    }
                }
            }
            i_3 = (long)((long)(i_3) + 1L);
        }
        return res_1;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(_p(three_sum(((long[])(new long[]{-1, 0, 1, 2, -1, -4})))));
            System.out.println(_p(three_sum(((long[])(new long[]{1, 2, 3, 4})))));
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
