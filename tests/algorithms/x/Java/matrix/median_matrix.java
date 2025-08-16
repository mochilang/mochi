public class Main {
    static long[][] matrix1 = ((long[][])(new long[][]{new long[]{1, 3, 5}, new long[]{2, 6, 9}, new long[]{3, 6, 9}}));
    static long[][] matrix2 = ((long[][])(new long[][]{new long[]{1, 2, 3}, new long[]{4, 5, 6}}));

    static long[] bubble_sort(long[] a) {
        long[] arr = ((long[])(a));
        long n_1 = (long)(arr.length);
        long i_1 = 0L;
        while ((long)(i_1) < (long)(n_1)) {
            long j_1 = 0L;
            while ((long)((long)(j_1) + 1L) < (long)((long)(n_1) - (long)(i_1))) {
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

    static long median(long[][] matrix) {
        long[] linear = ((long[])(new long[]{}));
        long i_3 = 0L;
        while ((long)(i_3) < (long)(matrix.length)) {
            long[] row_1 = ((long[])(matrix[(int)((long)(i_3))]));
            long j_3 = 0L;
            while ((long)(j_3) < (long)(row_1.length)) {
                linear = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(linear), java.util.stream.LongStream.of((long)(row_1[(int)((long)(j_3))]))).toArray()));
                j_3 = (long)((long)(j_3) + 1L);
            }
            i_3 = (long)((long)(i_3) + 1L);
        }
        long[] sorted_1 = ((long[])(bubble_sort(((long[])(linear)))));
        long mid_1 = (long)((long)(((long)(sorted_1.length) - 1L)) / 2L);
        return sorted_1[(int)((long)(mid_1))];
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(_p(median(((long[][])(matrix1)))));
            System.out.println(_p(median(((long[][])(matrix2)))));
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
