public class Main {

    static long[] reverse(long[] xs) {
        long[] res = ((long[])(new long[]{}));
        long i_1 = xs.length - 1;
        while (i_1 >= 0) {
            res = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(res), java.util.stream.LongStream.of(xs[(int)(i_1)])).toArray()));
            i_1 = i_1 - 1;
        }
        return res;
    }

    static long[] factors_of_a_number(long num) {
        long[] facs = ((long[])(new long[]{}));
        if (num < 1) {
            return facs;
        }
        long[] small_1 = ((long[])(new long[]{}));
        long[] large_1 = ((long[])(new long[]{}));
        long i_3 = 1L;
        while (i_3 * i_3 <= num) {
            if (Math.floorMod(num, i_3) == 0) {
                small_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(small_1), java.util.stream.LongStream.of(i_3)).toArray()));
                Object d_1 = Math.floorDiv(num, i_3);
                if (((Number)(d_1)).intValue() != i_3) {
                    large_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(large_1), java.util.stream.LongStream.of(((Number)(d_1)).longValue())).toArray()));
                }
            }
            i_3 = i_3 + 1;
        }
        facs = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(small_1), java.util.Arrays.stream(reverse(((long[])(large_1))))).toArray()));
        return facs;
    }

    static void run_tests() {
        if (!java.util.Arrays.equals(factors_of_a_number(1L), new long[]{1})) {
            throw new RuntimeException(String.valueOf("case1 failed"));
        }
        if (!java.util.Arrays.equals(factors_of_a_number(5L), new long[]{1, 5})) {
            throw new RuntimeException(String.valueOf("case2 failed"));
        }
        if (!java.util.Arrays.equals(factors_of_a_number(24L), new long[]{1, 2, 3, 4, 6, 8, 12, 24})) {
            throw new RuntimeException(String.valueOf("case3 failed"));
        }
        if (!java.util.Arrays.equals(factors_of_a_number(-24), new long[]{})) {
            throw new RuntimeException(String.valueOf("case4 failed"));
        }
    }

    static void main() {
        run_tests();
        System.out.println(_p(factors_of_a_number(24L)));
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
            if (d == Math.rint(d)) return String.valueOf((long) d);
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
