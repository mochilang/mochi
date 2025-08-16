public class Main {

    static boolean contains(long[] xs, long value) {
        long i = 0L;
        while ((long)(i) < (long)(xs.length)) {
            if ((long)(xs[(int)((long)(i))]) == (long)(value)) {
                return true;
            }
            i = (long)((long)(i) + 1L);
        }
        return false;
    }

    static long solution(long n) {
        long[] zmulti = ((long[])(new long[]{}));
        long[] xmulti_1 = ((long[])(new long[]{}));
        long temp_1 = 1L;
        while (true) {
            long result_1 = (long)(3L * (long)(temp_1));
            if ((long)(result_1) < (long)(n)) {
                zmulti = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(zmulti), java.util.stream.LongStream.of((long)(result_1))).toArray()));
                temp_1 = (long)((long)(temp_1) + 1L);
            } else {
                break;
            }
        }
        temp_1 = 1L;
        while (true) {
            long result_3 = (long)(5L * (long)(temp_1));
            if ((long)(result_3) < (long)(n)) {
                xmulti_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(xmulti_1), java.util.stream.LongStream.of((long)(result_3))).toArray()));
                temp_1 = (long)((long)(temp_1) + 1L);
            } else {
                break;
            }
        }
        long[] collection_1 = ((long[])(new long[]{}));
        long i_2 = 0L;
        while ((long)(i_2) < (long)(zmulti.length)) {
            long v_1 = (long)(zmulti[(int)((long)(i_2))]);
            if (!(Boolean)contains(((long[])(collection_1)), (long)(v_1))) {
                collection_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(collection_1), java.util.stream.LongStream.of((long)(v_1))).toArray()));
            }
            i_2 = (long)((long)(i_2) + 1L);
        }
        i_2 = 0L;
        while ((long)(i_2) < (long)(xmulti_1.length)) {
            long v_3 = (long)(xmulti_1[(int)((long)(i_2))]);
            if (!(Boolean)contains(((long[])(collection_1)), (long)(v_3))) {
                collection_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(collection_1), java.util.stream.LongStream.of((long)(v_3))).toArray()));
            }
            i_2 = (long)((long)(i_2) + 1L);
        }
        long total_1 = 0L;
        i_2 = 0L;
        while ((long)(i_2) < (long)(collection_1.length)) {
            total_1 = (long)((long)(total_1) + (long)(collection_1[(int)((long)(i_2))]));
            i_2 = (long)((long)(i_2) + 1L);
        }
        return total_1;
    }

    static void test_solution() {
        if ((long)(solution(3L)) != 0L) {
            throw new RuntimeException(String.valueOf("solution(3) failed"));
        }
        if ((long)(solution(4L)) != 3L) {
            throw new RuntimeException(String.valueOf("solution(4) failed"));
        }
        if ((long)(solution(10L)) != 23L) {
            throw new RuntimeException(String.valueOf("solution(10) failed"));
        }
        if ((long)(solution(600L)) != 83700L) {
            throw new RuntimeException(String.valueOf("solution(600) failed"));
        }
    }

    static void main() {
        test_solution();
        System.out.println("solution() = " + _p(solution(1000L)));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            main();
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
