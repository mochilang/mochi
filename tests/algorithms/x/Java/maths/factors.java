public class Main {

    static int[] reverse(int[] xs) {
        int[] res = ((int[])(new int[]{}));
        int i = xs.length - 1;
        while (i >= 0) {
            res = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res), java.util.stream.IntStream.of(xs[i])).toArray()));
            i = i - 1;
        }
        return res;
    }

    static int[] factors_of_a_number(int num) {
        int[] facs = ((int[])(new int[]{}));
        if (num < 1) {
            return facs;
        }
        int[] small = ((int[])(new int[]{}));
        int[] large = ((int[])(new int[]{}));
        int i_1 = 1;
        while (i_1 * i_1 <= num) {
            if (Math.floorMod(num, i_1) == 0) {
                small = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(small), java.util.stream.IntStream.of(i_1)).toArray()));
                int d = num / i_1;
                if (d != i_1) {
                    large = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(large), java.util.stream.IntStream.of(d)).toArray()));
                }
            }
            i_1 = i_1 + 1;
        }
        facs = ((int[])(concat(small, reverse(((int[])(large))))));
        return facs;
    }

    static void run_tests() {
        if (factors_of_a_number(1) != new int[]{1}) {
            throw new RuntimeException(String.valueOf("case1 failed"));
        }
        if (factors_of_a_number(5) != new int[]{1, 5}) {
            throw new RuntimeException(String.valueOf("case2 failed"));
        }
        if (factors_of_a_number(24) != new int[]{1, 2, 3, 4, 6, 8, 12, 24}) {
            throw new RuntimeException(String.valueOf("case3 failed"));
        }
        if (factors_of_a_number(-24) != ((Number)(new Object[]{})).intValue()) {
            throw new RuntimeException(String.valueOf("case4 failed"));
        }
    }

    static void main() {
        run_tests();
        System.out.println(_p(factors_of_a_number(24)));
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

    static <T> T[] concat(T[] a, T[] b) {
        T[] out = java.util.Arrays.copyOf(a, a.length + b.length);
        System.arraycopy(b, 0, out, a.length, b.length);
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
        return String.valueOf(v);
    }
}
