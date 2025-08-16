public class Main {

    static long[] subarray(long[] xs, long start, long end) {
        long[] result = ((long[])(new long[]{}));
        long k_1 = (long)(start);
        while ((long)(k_1) < (long)(end)) {
            result = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(result), java.util.stream.LongStream.of((long)(xs[(int)((long)(k_1))]))).toArray()));
            k_1 = (long)((long)(k_1) + 1L);
        }
        return result;
    }

    static long[] merge(long[] left_half, long[] right_half) {
        long[] result_1 = ((long[])(new long[]{}));
        long i_1 = 0L;
        long j_1 = 0L;
        while ((long)(i_1) < (long)(left_half.length) && (long)(j_1) < (long)(right_half.length)) {
            if ((long)(left_half[(int)((long)(i_1))]) < (long)(right_half[(int)((long)(j_1))])) {
                result_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(result_1), java.util.stream.LongStream.of((long)(left_half[(int)((long)(i_1))]))).toArray()));
                i_1 = (long)((long)(i_1) + 1L);
            } else {
                result_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(result_1), java.util.stream.LongStream.of((long)(right_half[(int)((long)(j_1))]))).toArray()));
                j_1 = (long)((long)(j_1) + 1L);
            }
        }
        while ((long)(i_1) < (long)(left_half.length)) {
            result_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(result_1), java.util.stream.LongStream.of((long)(left_half[(int)((long)(i_1))]))).toArray()));
            i_1 = (long)((long)(i_1) + 1L);
        }
        while ((long)(j_1) < (long)(right_half.length)) {
            result_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(result_1), java.util.stream.LongStream.of((long)(right_half[(int)((long)(j_1))]))).toArray()));
            j_1 = (long)((long)(j_1) + 1L);
        }
        return result_1;
    }

    static long[] merge_sort(long[] array) {
        if ((long)(array.length) <= 1L) {
            return array;
        }
        long middle_1 = (long)((long)(array.length) / 2L);
        long[] left_half_1 = ((long[])(subarray(((long[])(array)), 0L, (long)(middle_1))));
        long[] right_half_1 = ((long[])(subarray(((long[])(array)), (long)(middle_1), (long)(array.length))));
        long[] sorted_left_1 = ((long[])(merge_sort(((long[])(left_half_1)))));
        long[] sorted_right_1 = ((long[])(merge_sort(((long[])(right_half_1)))));
        return merge(((long[])(sorted_left_1)), ((long[])(sorted_right_1)));
    }

    static long h_index(long[] citations) {
        long idx = 0L;
        while ((long)(idx) < (long)(citations.length)) {
            if ((long)(citations[(int)((long)(idx))]) < 0L) {
                throw new RuntimeException(String.valueOf("The citations should be a list of non negative integers."));
            }
            idx = (long)((long)(idx) + 1L);
        }
        long[] sorted_1 = ((long[])(merge_sort(((long[])(citations)))));
        long n_1 = (long)(sorted_1.length);
        long i_3 = 0L;
        while ((long)(i_3) < (long)(n_1)) {
            if ((long)(sorted_1[(int)((long)((long)((long)(n_1) - 1L) - (long)(i_3)))]) <= (long)(i_3)) {
                return i_3;
            }
            i_3 = (long)((long)(i_3) + 1L);
        }
        return n_1;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(_p(h_index(((long[])(new long[]{3, 0, 6, 1, 5})))));
            System.out.println(_p(h_index(((long[])(new long[]{1, 3, 1})))));
            System.out.println(_p(h_index(((long[])(new long[]{1, 2, 3})))));
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
