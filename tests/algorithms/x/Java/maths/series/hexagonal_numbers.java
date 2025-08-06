public class Main {

    static int[] hexagonal_numbers(int length) {
        if (length <= 0) {
            throw new RuntimeException(String.valueOf("Length must be a positive integer."));
        }
        int[] res = ((int[])(new int[]{}));
        int n = 0;
        while (n < length) {
            res = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res), java.util.stream.IntStream.of(n * (2 * n - 1))).toArray()));
            n = n + 1;
        }
        return res;
    }

    static void test_hexagonal_numbers() {
        int[] expected5 = ((int[])(new int[]{0, 1, 6, 15, 28}));
        int[] result5 = ((int[])(hexagonal_numbers(5)));
        if (result5 != expected5) {
            throw new RuntimeException(String.valueOf("hexagonal_numbers(5) failed"));
        }
        int[] expected10 = ((int[])(new int[]{0, 1, 6, 15, 28, 45, 66, 91, 120, 153}));
        int[] result10 = ((int[])(hexagonal_numbers(10)));
        if (result10 != expected10) {
            throw new RuntimeException(String.valueOf("hexagonal_numbers(10) failed"));
        }
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            test_hexagonal_numbers();
            System.out.println(_p(hexagonal_numbers(5)));
            System.out.println(_p(hexagonal_numbers(10)));
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
        return String.valueOf(v);
    }
}
