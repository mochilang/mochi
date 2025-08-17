public class Main {

    static long[] two_pointer(long[] nums, long target) {
        long i = 0L;
        long j_1 = (long)((long)(nums.length) - 1L);
        while ((long)(i) < (long)(j_1)) {
            long s_1 = (long)((long)(nums[(int)((long)(i))]) + (long)(nums[(int)((long)(j_1))]));
            if ((long)(s_1) == (long)(target)) {
                return new long[]{i, j_1};
            }
            if ((long)(s_1) < (long)(target)) {
                i = (long)((long)(i) + 1L);
            } else {
                j_1 = (long)((long)(j_1) - 1L);
            }
        }
        return new long[]{};
    }

    static void test_two_pointer() {
        if (!java.util.Arrays.equals(two_pointer(((long[])(new long[]{2, 7, 11, 15})), 9L), new long[]{0, 1})) {
            throw new RuntimeException(String.valueOf("case1"));
        }
        if (!java.util.Arrays.equals(two_pointer(((long[])(new long[]{2, 7, 11, 15})), 17L), new long[]{0, 3})) {
            throw new RuntimeException(String.valueOf("case2"));
        }
        if (!java.util.Arrays.equals(two_pointer(((long[])(new long[]{2, 7, 11, 15})), 18L), new long[]{1, 2})) {
            throw new RuntimeException(String.valueOf("case3"));
        }
        if (!java.util.Arrays.equals(two_pointer(((long[])(new long[]{2, 7, 11, 15})), 26L), new long[]{2, 3})) {
            throw new RuntimeException(String.valueOf("case4"));
        }
        if (!java.util.Arrays.equals(two_pointer(((long[])(new long[]{1, 3, 3})), 6L), new long[]{1, 2})) {
            throw new RuntimeException(String.valueOf("case5"));
        }
        if ((long)(two_pointer(((long[])(new long[]{2, 7, 11, 15})), 8L).length) != 0L) {
            throw new RuntimeException(String.valueOf("case6"));
        }
        if ((long)(two_pointer(((long[])(new long[]{0, 3, 6, 9, 12, 15, 18, 21, 24, 27})), 19L).length) != 0L) {
            throw new RuntimeException(String.valueOf("case7"));
        }
        if ((long)(two_pointer(((long[])(new long[]{1, 2, 3})), 6L).length) != 0L) {
            throw new RuntimeException(String.valueOf("case8"));
        }
    }

    static void main() {
        test_two_pointer();
        System.out.println(two_pointer(((long[])(new long[]{2, 7, 11, 15})), 9L));
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
}
