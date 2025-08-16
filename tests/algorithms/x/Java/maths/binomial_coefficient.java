public class Main {

    static long binomial_coefficient(long n, long r) {
        if ((long)(n) < 0L || (long)(r) < 0L) {
            throw new RuntimeException(String.valueOf("n and r must be non-negative integers"));
        }
        if ((long)(n) == 0L || (long)(r) == 0L) {
            return 1;
        }
        long[] c_1 = ((long[])(new long[]{}));
        for (int _v = 0; _v < ((long)(r) + 1L); _v++) {
            c_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(c_1), java.util.stream.LongStream.of(0L)).toArray()));
        }
c_1[(int)(0L)] = 1L;
        long i_1 = 1L;
        while ((long)(i_1) <= (long)(n)) {
            long j_1 = (long)((long)(i_1) < (long)(r) ? i_1 : r);
            while ((long)(j_1) > 0L) {
c_1[(int)((long)(j_1))] = (long)((long)(c_1[(int)((long)(j_1))]) + (long)(c_1[(int)((long)((long)(j_1) - 1L))]));
                j_1 = (long)((long)(j_1) - 1L);
            }
            i_1 = (long)((long)(i_1) + 1L);
        }
        return c_1[(int)((long)(r))];
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(_p(binomial_coefficient(10L, 5L)));
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
